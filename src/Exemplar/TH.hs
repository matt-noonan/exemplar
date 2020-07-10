{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}

module Exemplar.TH where

import           Exemplar
import           Language.Haskell.TH
import Language.Haskell.TH.ExpandSyns
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Data.Functor
import Data.Char (isAlphaNum)
import Data.List (foldl', sort, sortOn, intercalate)
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import Debug.Trace
import Data.Ord (Down(..))

data TypeInfo = TypeInfo
  { originalType     :: Type
  , variables        :: [TyVarBndr]
  , unaryConstraints :: Map Name [Name]
  , arguments        :: [Type]
  , result           :: Type
  } deriving Show

tvName :: TyVarBndr -> Name
tvName = \case
    PlainTV n    -> n
    KindedTV n _ -> n

isOperator :: String -> Bool
isOperator "" = False
isOperator (c:_) = not (isAlphaNum c) && not (c `elem` "_'")

makeTyApp :: Name -> TypeInfo -> Map Name Type -> Maybe Exp
makeTyApp name ti ctx =
    go name' <$> traverse ((`Map.lookup` ctx) . tvName) (variables ti)
  where
    name' = let base = nameBase name in
        if isOperator base
        then ParensE (VarE name)
        else VarE name
    go :: Exp -> [Type] -> Exp
    go acc [] = acc
    go acc (t:ts) = go (AppTypeE acc t) ts
        
updateConstraints :: [Type] -> Map Name [Name] -> Maybe (Map Name [Name])
updateConstraints [] m = pure m
updateConstraints (c:cs) m = do
    case c of
        AppT (ConT clasz) (VarT v) -> do
            let m' = Map.insertWith (\new old -> old ++ new) v [clasz] m
            updateConstraints cs m'
        _ -> Nothing

splitArgsAndResult :: Type -> ([Type], Type)
splitArgsAndResult = go []
  where
    go args (AppT (AppT ArrowT x) y) = go (x : args) y
    go args t = (reverse args, t)
    
parseTypeInfo :: Type -> Maybe TypeInfo
parseTypeInfo tt = go (TypeInfo tt [] Map.empty [] undefined) tt
  where
    go :: TypeInfo -> Type -> Maybe TypeInfo
    go ti = \case
        ForallT tvs ctx body -> do
          unaryConstraints' <- updateConstraints ctx (unaryConstraints ti)
          go ti { variables = variables ti ++ tvs
                , unaryConstraints = unaryConstraints' } body
        AppT (AppT ArrowT x) y -> do
            go ti { arguments = arguments ti ++ [x] } y
        ParensT x    -> go ti x
        t@(AppT _ _) -> pure ti { result = t }
        t@(ConT _)   -> pure ti { result = t }
        t@(VarT _)   -> pure ti { result = t }
        _            -> Nothing
        
unlist :: Type -> [Type]
unlist = \case
    SigT t _     -> unlist t
    ParensT t    -> unlist t
    PromotedNilT -> []
    AppT (AppT PromotedConsT x) xs -> x : unlist xs
    _ -> []

substituteBinders :: Map Name Type -> [TyVarBndr] -> [TyVarBndr]
substituteBinders m = filter (not . (`Map.member` m) . tvName)

substitute :: Map Name Type -> Type -> Type
substitute m = go
  where
    go = \case
        VarT n    -> case Map.lookup n m of
                        Just ty -> ty
                        Nothing -> VarT n
        ParensT t -> ParensT (go t)
        AppT x y  -> AppT (go x) (go y)
        ForallT tvs ctx body ->
          let tvs' = substituteBinders m tvs
              ctx' = map (substitute m) ctx
              body' = substitute m body
          in if null tvs' then body' else ForallT tvs' ctx' body'
        -- FIXME: there are a few more recursive positions to handle
        etc       -> etc

toAssignments :: Map Name [Type] -> [Map Name Type]
toAssignments = go . Map.toList
  where
    go :: [(Name, [Type])] -> [Map Name Type]
    go = \case
        []              -> [Map.empty]
        ((n, ts) : etc) -> map (Map.insert n) ts <*> go etc

data Fst a b = Fst a b

instance Eq a => Eq (Fst a b) where
    Fst x _ == Fst y _ = x == y
    
instance Ord a => Ord (Fst a b) where
    compare (Fst x _) (Fst y _) = compare x y
    
unFst :: Fst a b -> b
unFst (Fst _ x) = x

-- | Filter a list of instance declarations and extract the type
-- that each instance is defined over.
--
-- NOTE: This is a little different from normal instance resolution!
-- For example, the instance @Example (Int -> String)@ should be
-- returned when searching for instances of @Example (a -> b)@.
exampleInstances :: Int -> String -> Type -> Q [Map Name Type]
exampleInstances _ _ (VarT n) =
    map (Map.singleton n) <$> findExemplars ''Example
exampleInstances depth caller t0_ = do
    t0 <- expandSyns t0_
    -- Look up every possible instance of Example here, and trawl through
    -- them for instances that can unify with a specialization of t0.
    -- Return those instance heads, unified with t0.
    let anything = VarT (mkName "any'example")
    instances <- reifyInstances ''Example [anything]
    sortOn (complexity . (`substitute` t0)) . concat <$> mapM (go t0) instances
  where
    go :: Type -> InstanceDec -> Q [Map Name Type]
    go t0 etc =  case etc of
        -- An Example instance with no constraints. See if the instance
        -- head can be unified with t0.
        InstanceD _ [] (AppT _ tx) _ -> do
          t <- expandSyns tx
          pure $ case unify t0 t of
              Nothing     -> [] -- unreachable?
              Just (m, _) -> [m]

        InstanceD _ cs0 (AppT h tx) _ | depth > 0 -> do
           --runIO (putStrLn $ "  (i): " ++ pprint tx)
           t <- expandSyns tx
           case unify t0 t of
              Nothing -> pure [] -- unreachable?
              Just (m, m') -> do
                  -- TODO: ensure that the class is Example
                  t0' <- expandSyns (substitute m t0)
                  let cs = [c | AppT (ConT _) c <- map (substitute m') cs0]
                  instanceVars <- squash <$> forM cs
                                   (exampleInstances (depth - 1) (pprint t0' ++ "@" ++ pprint h))
                  -- FIXME: m and m'' used to be swapped in the next line, but there
                  -- were cases where m was empty. Is substituting even correct here?
                  pure $ map (\m'' -> Map.map (substitute m) m'') instanceVars

        _  -> pure []

-- | Union two maps, if they agree on their common keys.
maybeUnion :: Maybe (Map Name Type) -> Maybe (Map Name Type) -> Maybe (Map Name Type)
maybeUnion Nothing _ = Nothing
maybeUnion _ Nothing = Nothing
maybeUnion (Just x) (Just y) =
  let same (Just n) (Just m) | n == m = Just n
      same _ _ = Nothing
  in sequence (Map.unionWith same (Map.map Just x) (Map.map Just y))

maybeUnion2 :: Maybe (Map Name Type, Map Name Type)
            -> Maybe (Map Name Type, Map Name Type)
            -> Maybe (Map Name Type, Map Name Type)
maybeUnion2 lhs rhs =
  (,) <$> (maybeUnion (fst <$> lhs) (fst <$> rhs))
      <*> (maybeUnion (snd <$> lhs) (snd <$> rhs))

squash :: (Ord k, Eq v) => [[Map k v]] -> [Map k v]
squash = go Map.empty
  where
    go acc = \case
        []           -> [acc]
        ((m:ms):mss) | isCompatible acc m -> go (Map.union acc m) mss ++ go acc (ms:mss)
        ((m:ms):mss) -> go acc (ms:mss)
        ([]:_)       -> []

-- | Measure the complexity of a type, so that simpler types can be selected
-- before more complex types.
complexity :: Type -> Int
complexity = \case
    VarT _ -> 1
    ConT _ -> 1
    AppT x y -> 1 + complexity x + complexity y
    AppKindT x k -> 1 + complexity x + complexity k
    ListT -> 0
    SigT t k -> 1 + complexity t + complexity k
    ArrowT -> 0
    TupleT _ -> 0
    ParensT x -> complexity x
    EqualityT -> 0
    StarT -> 0
    ConstraintT -> 0
    _     -> 1000

-- | Sort a list of types, most-complex first, removing duplicates.
byDescendingComplexity :: [Type] -> [Type]
byDescendingComplexity = sortOn (Down . complexity) . filterRepeats

-- | `unify lhs rhs` returns the name -> type mapping m
-- that makes @substitute m lhs ~ rhs@.
unify :: Type -> Type -> Maybe (Map Name Type, Map Name Type)
unify t (VarT n) = Just (Map.empty, Map.singleton n t)
unify (VarT n) t = Just (Map.singleton n t, Map.empty)
unify (AppT x y) (AppT x' y') = maybeUnion2 (unify x x') (unify y y')
unify ListT ListT = Just (Map.empty, Map.empty)
unify (AppKindT t k) (AppKindT t' k') = maybeUnion2 (unify t t') (unify k k')
unify (SigT t k) (SigT t' k') = maybeUnion2 (unify t t') (unify k k')
unify (SigT t _) t' = unify t t'
unify t (SigT t' _) = unify t t'
unify ArrowT ArrowT = Just (Map.empty, Map.empty)
unify (ConT n) (ConT n') | n == n' = Just (Map.empty, Map.empty)
unify (TupleT n) (TupleT n') | n == n' = Just (Map.empty, Map.empty)
unify (ParensT t) t' = unify t t'
unify t (ParensT t') = unify t t'
unify EqualityT EqualityT = Just (Map.empty, Map.empty)
unify StarT StarT = Just (Map.empty, Map.empty)
unify ConstraintT ConstraintT = Just (Map.empty, Map.empty)
unify _ _ = Nothing

hasFreeVariables :: Type -> Bool
hasFreeVariables = \case
    VarT _ -> True
    ConT _ -> False
    AppT x y -> hasFreeVariables x || hasFreeVariables y
    AppKindT x k -> hasFreeVariables x || hasFreeVariables k
    ListT -> False
    SigT t k -> hasFreeVariables t || hasFreeVariables k
    ArrowT -> False
    TupleT _ -> False
    ParensT x -> hasFreeVariables x
    EqualityT -> False
    StarT -> False
    ConstraintT -> False
    _     -> False

-- | Check if two maps agree on their common keys
isCompatible :: (Ord k, Eq v) => Map k v -> Map k v -> Bool
isCompatible m m' = all snd (Map.toList $ Map.intersectionWith (==) m m')


-- | Given a type, possibly with free type variables, find all possible
-- assignments to those type variables which allow the result to have
-- an `Example` instance.
findExampleVariables :: Type -> Q [Map Name Type]
findExampleVariables = fmap filterRepeats . exampleInstances 1 "[toplevel]"
    
-- | This is the same as 'nub', except it uses an Ord instance for efficiency.
filterRepeats :: (Ord a, Eq a) => [a] -> [a]
filterRepeats = go Set.empty
  where
    go _ [] = []
    go seen (x:xs) | x `Set.member` seen = go seen xs
    go seen (x:xs) = x : go (Set.insert x seen) xs
    
forLimit :: Monad m => Int -> ([b] -> Int) -> [a] -> (a -> m b) -> m [b]
forLimit limit measure todo action = go [] todo
  where
    go acc [] = pure (reverse acc)
    go acc (x:xs) | measure acc >= limit = pure (reverse acc)
    go acc (x:xs) = do
        result <- action x
        go (result : acc) xs
        
-- | Given an assignment of type variables to types, and a list of types,
-- find all possible variable-to-type assignments that extend the original
-- and allow each type in the list to have an `Example` instance.
findAllExamples :: Int -> Map Name Type -> [Type] -> Type -> Q (Set (Map Name Type))
findAllExamples limit m (t:ts) result = do
    ms <- mapMaybe (maybeUnion (Just m) . Just) <$> findExampleVariables (substitute m t)
    Set.unions <$> forLimit limit (Set.size . Set.unions) ms (\m' -> findAllExamples limit m' ts result)
findAllExamples limit m [] result = do
    -- If the current variable assignments have made the result type a function,
    -- then we will need to provide extra arguments. Keep going, you're not done yet!
    case splitArgsAndResult (substitute m result) of
        ([], _)     -> pure (Set.singleton m)
        (args, res) -> findAllExamples limit m (byDescendingComplexity args) res 

-- | Given the name of a typeclass (of kind * -> Constraint),
-- find the list of exemplars for this class.
-- This is the same as the list of types you get from the type
-- family `Exemplar c`
findExemplars :: Name -> Q [Type]
findExemplars c = do
    reifyInstances ''Exemplar [ConT c] <&> \case
      [TySynInstD (TySynEqn _ _ ty)] -> unlist ty
      _                              -> []

-- | Given a list of constraint names, produce a list of types
-- that are exemplars for all of those constraints. Sort them
-- in the order in which they appear in the first constraint, on
-- the theory that the first constraint is the most interesting one
findCommonExemplars :: [Name] -> Q [Type]
findCommonExemplars cs = do
    exemplars <- mapM findExemplars cs
    let common = case map Set.fromList exemplars of
                     []     -> Set.empty
                     (i:is) -> foldl' Set.intersection i is
    pure $ case exemplars of
             [] -> []
             (e:_) -> filter (`Set.member` common) e

assignmentScore :: Map Name Type -> Int
assignmentScore m =
  let types = Map.elems m
  in sum (map complexity types) - Set.size (Set.fromList types)

-- | Given the name and type information for a function, produce an
-- IO action that executes many input/output examples for that function.
exemplarsOf :: Name -> Q Exp
exemplarsOf name = reify name >>= \info -> do
  let mti = case info of
               ClassOpI _ tt _ -> parseTypeInfo tt
               VarI _ tt _     -> parseTypeInfo tt
               DataConI _ tt _ -> parseTypeInfo tt
               _               -> Nothing
  case mti of
    Nothing -> fail ("Could not get type information for " ++ show name ++ ", info is " ++ show info)
    Just ti -> do
      ms <- toAssignments <$> traverse findCommonExemplars (unaryConstraints ti)

      when (null ms) $
          fail ("Could not find any common exemplars for the constraints:\n" ++
                show (unaryConstraints ti) ++ "\n")

      --runIO (putStrLn $ "Possible exemplar assignments:")
      --forM_ ms $ \m ->
      --    runIO (putStrLn $ "    " ++ show (Map.toList $ Map.map pprint m))
      --runIO (putStrLn $ "arguments>>> " ++ show (map pprint $ arguments ti) )
      --runIO (putStrLn $ "result>>> " ++ pprint (result ti))
      
      body <- fmap concat . forM ms $ \m' -> do
        examples <- findAllExamples 5 m' (byDescendingComplexity $ arguments ti) (result ti)

        let compats = sortOn assignmentScore (Set.toList examples)
        
        when (null compats) $
          runIO (putStrLn $ "Could not find compatible examples for " ++ pprint (substitute m' (originalType ti)))
        nn <- niceName name
        fmap concat . forM compats $ \compat -> do
          let moreVars = Set.toList (Set.fromList (map tvName (variables ti))
                                       `Set.difference` Set.fromList (Map.keys compat))
        
          let mkEx = AppE (VarE 'makeExamples) (ArithSeqE (FromToR (LitE (IntegerL 0))
                                                                   (LitE (IntegerL 5))))
              sayEach tyapp = AppE (AppE (VarE '(=<<)) (AppE (VarE 'mapM_) (VarE 'putStrLn)))
                (AppE
                 (AppE (VarE 'fmap) (VarE 'filterRepeats))
                 (AppE (AppE mkEx tyapp) (LitE (StringL nn))))
              prettyName = if isOperator (nameBase name)
                           then "(" ++ nameBase name ++ ")"
                           else nameBase name
              hdr = [ AppE (VarE 'putStrLn) (LitE (StringL "------------------------------------"))
                    , AppE (VarE 'putStrLn) (LitE (StringL ("-- " ++ prettyName ++ " :: " ++ (basePrint $ substitute compat (originalType ti)))))
                    , AppE (VarE 'putStrLn) (LitE (StringL "------------------------------------"))
                    ]
              ftr = [ AppE (VarE 'putStrLn) (LitE (StringL "\n")) ]
              
          pure $ case sayEach <$> makeTyApp name ti compat of
                   Nothing -> []
                   Just e  -> hdr ++ [e] ++ ftr

      pure (DoE (map NoBindS (body ++ [AppE (VarE 'pure) (TupE [])])))

niceName :: Name -> Q String
niceName name = do
  let n = nameBase name
      paren x = "(" ++ x ++ ")"
      isOperator x = length x > 0 && not (isAlphaNum (head x)) && not (elem (head x) "'_")
  reifyFixity name <&> \case
      Nothing -> if isOperator n then paren n else n
      Just _  -> paren (if isOperator n then n else "`" ++ n ++ "`")
      
  
monomorphize :: Name -> Q [Dec]
monomorphize name = do
    body <- exemplarsOf name
    let outName = mkName ("exemplars_of_" ++ nameBase name)
    io <- [t|IO ()|]
    pure [ SigD outName io
         , FunD outName [Clause [] (NormalB body) [] ]]

basePrint :: Type -> String
basePrint tt = intercalate " -> " (map basePrintP (args ++ [result]))
  where
    (args, result) = splitArgsAndResult tt
    str = \case
        VarT t -> pprint t
        ConT n ->
          let nm = nameBase n
          in if nm == "FakeIO" then "IO" else nm
        AppT ListT x ->
          let s = "[" ++ basePrint x ++ "]"
          in if s == "[Char]" then "String" else s
        AppT x y -> unwords [basePrintP x, basePrintP y]
        AppKindT x k -> basePrintP x ++ " @" ++ basePrintP k
        SigT t k -> unwords [basePrintP t, "::", basePrintP k]
        ArrowT -> "->"
        TupleT n -> "(" ++ replicate n ',' ++ ")"
        ParensT x -> "(" ++ basePrint x ++ ")"
        EqualityT -> "~"
        StarT -> "Type"
        ConstraintT -> "Constraint"
        s     -> "{{{" ++ show s ++ "}}}"
    basePrintP t =
      let (args', result') = splitArgsAndResult t
      in case args' of
          [] -> str result'
          _  -> "(" ++ intercalate " -> " (map basePrintP (args' ++ [result'])) ++ ")"

