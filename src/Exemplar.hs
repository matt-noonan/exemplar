{-# language PolyKinds, UndecidableInstances #-}

module Exemplar where

import Data.Kind
import Data.List

import Data.IORef
import Data.Char
import Control.Exception
import Text.Read (readMaybe)
import Data.Maybe (listToMaybe)
import Data.Foldable
import Data.Traversable
import Data.Proxy
import System.Timeout
import Data.Ord (Down(..))
import Data.Typeable
import Control.Monad
import Control.Applicative hiding (Const)

data Symbolic
  = Const Integer
  | Var Char
  | Plus Symbolic Symbolic
  | Times Symbolic Symbolic
  | Minus Symbolic Symbolic
  | Abs Symbolic
  | Signum Symbolic

instance Num Symbolic where
  fromInteger = Const
  (+) = Plus
  (*) = Times
  (-) = Minus
  abs = Abs
  signum = Signum

instance Show Symbolic where
  show = \case
      Var c     -> c : ""
      Const n   -> show n
      Plus x y  -> "(" ++ show x ++ " + " ++ show y ++ ")"
      Times x y -> "(" ++ show x ++ " * " ++ show y ++ ")"
      Minus x y -> "(" ++ show x ++ " - " ++ show y ++ ")"
      Abs x     -> "(abs " ++ show x ++ ")"
      Signum x  -> "(signum " ++ show x ++ ")"

data Suit = Spades | Hearts | Clubs | Diamonds deriving (Eq, Ord, Show, Bounded, Enum)

type family Exemplar (t :: k -> Constraint) :: [k]

type instance Exemplar Functor =
    [[], Maybe, Either Int, FakeIO, (->) Int]

type instance Exemplar Applicative =
    [Maybe, Either Int, FakeIO, [], (->) Int]

type instance Exemplar Monad =
    [FakeIO, Maybe, Either Int, [], (->) Int]

type instance Exemplar Num = [Int, Symbolic]

type instance Exemplar Eq = [Int, String, [Int]]

type instance Exemplar Ord = [Int, String, [Int], Down Int]

type instance Exemplar Bounded = [Bool, Suit]

type instance Exemplar Enum = [Suit, Bool, Char, Int]

type instance Exemplar Foldable =  [[], Maybe]

type instance Exemplar Traversable = [[], Maybe]

type instance Exemplar Alternative =
    '[ Maybe ]

type instance Exemplar Monoid =
    [ String, [Int] ]
    
type instance Exemplar Example =
    [ Int, Symbolic, String, Char, Bool ]

data Display
  = Unshowable String
  | UhOh String
  | Result String
  | List [Display]
  | App [Display]
  | Section String Display
  | Tuple [Display]
  | WithSideEffects [Action] Display
  | More

app :: Display -> Display -> Display
app (App xs) t = App (xs ++ [t])
app x y = App [x,y]

isInfix :: String -> Bool
isInfix ('(':x) = True
isInfix _       = False

toInfix :: String -> String
toInfix ('(':x) = init x

instance Show Display where
    show = \case
        Unshowable t -> "(_ :: " ++ t ++ ")"
        UhOh msg     -> "*** Exception: " ++ msg
        Result s     -> s
        List ds      -> "[" ++ intercalate ", " (map show ds) ++ "]"
        Tuple ds     -> "(" ++ intercalate ", " (map show ds) ++ ")"
        App [Result o, x] | isInfix o -> "(" ++ show x ++ " " ++ toInfix o ++ ")"
        App [Result o, x, y] | isInfix o -> unwords [show x, toInfix o, show y]
        App (Result o : x : y : args) | isInfix o ->
           unwords (("(" ++ intercalate " " [show x, toInfix o, show y] ++ ")") : map showP args)
        App ds       -> unwords (map showP ds)
        Section op x -> "(" ++ op ++ " " ++ show x ++ ")"
        WithSideEffects actions d -> "[" ++ show actions ++ "]" ++ show d
        More -> "..."

showP :: Display -> String
showP orig = case orig of
    Result s ->
      case readMaybe s :: Maybe Int of
             Just n | n < 0 -> "(" ++ s ++ ")"
             _              -> s
    List _   -> show orig
    Tuple _  -> show orig
    UhOh _   -> show orig
    Unshowable _ -> show orig
    Section _ _  -> show orig
    App _        -> "(" ++ show orig ++ ")"
    WithSideEffects actions d -> "(" ++ show orig ++ ")"
    More -> "(...)"
    
hasUnshowableParts :: Display -> Bool
hasUnshowableParts = \case
    Unshowable _ -> True
    UhOh _       -> False
    Result _     -> False
    List ds      -> any hasUnshowableParts ds
    Tuple ds     -> any hasUnshowableParts ds
    App ds       -> any hasUnshowableParts ds
    WithSideEffects _ d -> hasUnshowableParts d
    More         -> False
    
class Displayable t where
    display :: t -> Display
    default display :: Show t => t -> Display
    display = Result . show 

instance {-# overlappable #-} Typeable t => Displayable t where
    display = Unshowable . show . typeOf

data ExampleOf t = ExampleOf
  { value :: t
  , displayed :: Display
  }

instance Show (ExampleOf t) where
    show = show . displayed

instance {-# overlapping #-} Displayable (ExampleOf t) where
    display = displayed

shown :: Show t => t -> ExampleOf t
shown x = ExampleOf { value = x, displayed = Result (show x) }

class Example t where
    examples :: [ExampleOf t]

instance Example Bool where
    examples = map shown [True, False]
    
instance Example Int where
    examples = map shown [0, 1, -1, 3, 4, 10, 17, 19, 100, -42]

instance Example (Down Int) where
    examples = map (\e -> e { value = Down (value e)
                            , displayed = app (Result "Down") (displayed e) }) examples
               
instance Example Char where
    examples = map shown "abcdABCD0123456789!@#$%^&*"

instance Example Suit where
    examples = map shown [minBound..maxBound]
    
instance {-# overlapping #-} Displayable Bool
instance {-# overlapping #-} Displayable Int
instance {-# overlapping #-} Displayable (Down Int)
instance {-# overlapping #-} Displayable Char
instance {-# overlapping #-} Displayable Symbolic
instance {-# overlapping #-} Displayable ()
instance {-# overlapping #-} Displayable String
instance {-# overlapping #-} Displayable Suit
instance {-# overlapping #-} Displayable a => Displayable [a] where
    display xs = List (map display xs)
instance  {-# overlapping #-} Displayable a => Displayable (Maybe a) where
    display = \case
        Nothing -> Result "Nothing"
        Just x  -> app (Result "Just") (display x)
instance {-# overlapping #-} (Displayable a, Displayable b) => Displayable (Either a b) where
    display = \case
        Left  x -> app (Result "Left") (display x)
        Right y -> app (Result "Right") (display y)
instance {-# overlapping #-} (Displayable a, Displayable b) => Displayable (a, b) where
    display (x,y) = Tuple [display x, display y]
instance {-# overlapping #-} (Displayable a, Displayable b, Displayable c) => Displayable (a,b,c) where
    display (x,y,z) = Tuple [display x, display y, display z]
instance {-# overlapping #-} (Displayable a, Displayable b, Displayable c, Displayable d) => Displayable (a,b,c,d) where
    display (x,y,z,w) = Tuple [display x, display y, display z, display w]

instance {-# overlapping #-} Example String where
    examples = map shown
      [ "Hello"
      , "world!"
      , "1234"
      , "foo"
      , "bar"
      , "The quick brown fox"
      ]

instance (Example a, Example b) => Example (Either a b) where
    examples =
      let blend [] ys = ys
          blend xs [] = xs
          blend (x:xs) (y:ys) = x : y : blend xs ys
          lefts = [ ExampleOf { value = Left (value x)
                              , displayed = app (Result "Left") (displayed x) }
                  | x <- examples ]
          rights = [ ExampleOf { value = Right (value x)
                               , displayed = app (Result "Right") (displayed x) }
                   | x <- examples ]
      in blend lefts rights


instance (Example a, Example b) => Example (a, b) where
    examples =
      [ ExampleOf { value = (value x, value y)
                  , displayed = Tuple [displayed x, displayed y] }
      | x <- examples, y <- examples ]


instance Example Symbolic where
    examples = map toExampleOf "abcdxyzwpqrst"
      where
        toExampleOf c = ExampleOf { value = Var c, displayed = Result (c : "") }

instance {-# overlappable #-} Example t => Example (Maybe t) where
    examples =
      ExampleOf { value = Nothing, displayed = Result "Nothing" } : map withJust examples
      where
        withJust e = ExampleOf { value = Just (value e)
                               , displayed = app (Result "Just") (displayed e) }

instance {-# overlapping #-} Example (Maybe Int) where
    examples =
      ExampleOf { value = Nothing, displayed = Result "Nothing" } : map withJust examples
      where
        withJust e = ExampleOf { value = Just (value e)
                               , displayed = app (Result "Just") (displayed e) }

pickExample :: Example t => Int -> ExampleOf t
pickExample n = cycle examples !! n

instance {-# overlappable #-} Example a => Example [a] where
    examples =
      let listOf es = ExampleOf { value     = map value es
                                , displayed = List (map displayed es) }
      in [ listOf []
         , listOf [pickExample 0, pickExample 1, pickExample 2]
         , listOf [pickExample 3, pickExample 4]
         , listOf [pickExample 5]
         , listOf [pickExample 6, pickExample 8, pickExample 7]
         ]

instance {-# overlapping #-} Example [String] where
    examples = [ ExampleOf { value = [], displayed = List [] }
               , ExampleOf { value = ["Hello", "world!"]
                           , displayed = List [ Result "\"Hello\""
                                              , Result "\"world!\"" ] }
               ]
      
instance Example (Int -> Int -> Int) where
    examples =
      [ ExampleOf { value = (+), displayed = Result "(+)" }
      , ExampleOf { value = (*), displayed = Result "(*)" }
      , ExampleOf { value = const, displayed = Result "const" }
      ]

instance Example (Symbolic -> Symbolic -> Symbolic) where
    examples =
      [ ExampleOf { value = (+), displayed = Result "(+)" }
      , ExampleOf { value = (*), displayed = Result "(*)" }
      , ExampleOf { value = const, displayed = Result "const" }
      ]

instance Example (FakeIO () -> FakeIO () -> FakeIO ()) where
    examples =
      [ ExampleOf { value = (>>), displayed = Result "(>>)" }
      ]

{-
instance {-# overlappable #-} Example a => Example (Int -> [a] -> [a]) where
    examples =
      [ ExampleOf { value = take, displayed = Result "take" }
      , ExampleOf { value = drop, displayed = Result "drop" }
      ]
-}

instance Example (String -> String) where
    examples =
      [ ExampleOf { value = reverse, displayed = Result "reverse" }
      , ExampleOf { value = map Data.Char.toUpper, displayed = app (Result "map") (Result "toUpper") }
      ]

instance Example (String -> String -> String) where
    examples =
      [ ExampleOf { value = (++), displayed = Result "(++)" }
      , ExampleOf { value = flip (++), displayed = app (Result "flip") (Result "(++)") }
      , ExampleOf { value = const, displayed = Result "const" }
      ]

instance Example (String -> Int) where
    examples =
      [ ExampleOf { value = length, displayed = Result "length" }
      ]

instance Example (Int -> Bool) where
    examples =
      [ ExampleOf { value = even, displayed = Result "even" }
      , ExampleOf { value = (== 1), displayed = Section "==" (Result "1") }
      , ExampleOf { value = (> 7), displayed = Section ">" (Result "7") }
      ]

instance Example (Int -> Int) where
    examples =
      [ ExampleOf { value = (+ 1), displayed = Section "+" (Result "1") }
      ]

instance Example (Bool -> String) where
    examples =
      [ ExampleOf { value = show, displayed = Result "show" }
      , ExampleOf { value = show . not, displayed = App [ Result "(.)"
                                                        , Result "show"
                                                        , Result "not" ] }
      ]

instance Example (Int -> String) where
    examples =
      [ ExampleOf { value = show, displayed = Result "show" }
      ]

instance Example (String -> Maybe Int) where
    examples =
      [ ExampleOf { value = readMaybe, displayed = Result "readMaybe @Int" }
      ]

instance Example ([Int] -> Maybe Int) where
    examples =
      [ ExampleOf { value = listToMaybe, displayed = Result "listToMaybe" }
      ]

perform :: [Action] -> a -> FakeIO a
perform actions x = FakeIO $
  \(FakeIOState n s) -> (FakeIOState n (s ++ actions), Just x)

instance Example (String -> FakeIO ()) where
    examples =
      [ ExampleOf { value = \s -> perform [Output s] ()
                  , displayed = Result "putStrLn" }
      ]

instance Example (FakeIO ()) where
    examples =
      ExampleOf { value = pure ()
                , displayed = app (Result "pure") (Result "()") }
      : map (\e -> ExampleOf { value = perform [Output (value e)] ()
                             , displayed = app (Result "putStrLn") (displayed e) })
        examples

instance Example (FakeIO String) where
    examples =
      [ ExampleOf { value = perform [Input "test"] "test"
                  , displayed = Result "getLine" }
      ]
      
data Action
  = Output String
  | Input String
  | KeepOnTruckin'
  deriving Show

data FakeIOState = FakeIOState Int [Action]

newtype FakeIO a = FakeIO { runFakeIO :: FakeIOState -> (FakeIOState, Maybe a) } deriving Functor

instance Applicative FakeIO where
    pure x = FakeIO (\s -> (s, Just x))
    FakeIO f <*> FakeIO x = FakeIO $ \s ->
        let (s', f') = f s
            (s'', x') = x s'
        in (s'', f' <*> x')

instance Monad FakeIO where
    FakeIO mx >>= f = FakeIO $ \s ->
        let (FakeIOState n actions, x) = mx s
            s' = FakeIOState (n - 1) actions
        in if n < 0 then (s', Nothing)
           else case x of
                    Nothing -> (s', Nothing)
                    Just x' -> let FakeIO y = f x' in y s'

instance Displayable a => Displayable (FakeIO a) where
    display (FakeIO f) =
      let (FakeIOState _ actions, x) = f (FakeIOState 10 [])
      in WithSideEffects actions (maybe More display x)
    
class MakeExample t where
    makeExample_ :: Display -> Int -> t -> IO (Int, Display, Either SomeException Display)

type family Result t where
    Result (a -> b)   = Result b
    Result a          = a

data Timeout = Timeout
instance Show Timeout where show _ = "<<timeout>>"
instance Exception Timeout

evaluate' :: Displayable a => a -> IO Display
evaluate' v =
    timeout 5 (evaluate (display v)) >>= \case
        Just x  -> pure x
        Nothing -> throw Timeout

instance {-# overlapping #-} (Example a, MakeExample (b -> c))
  => MakeExample (a -> b -> c) where
    makeExample_ ans i f = do
        let e = pickExample i
            ans' = app ans (display e)
        (i', ans'', result) <- makeExample_ ans' (i + 1) (f (value e))
                           `catch` (\(ex :: SomeException) -> pure (i, ans', Left ex))
        pure (i', ans'', result)

instance {-# overlaps #-} (Displayable b, Example a, Result (a -> b) ~ b) 
  => MakeExample (a -> b) where
    makeExample_ ans i f = do
        let e = pickExample i
            ans' = app ans (display e)
        result <- (Right <$> evaluate' (f (value e)))
                  `catch` (\(ex :: SomeException) -> pure (Left ex))
        pure (i + 1, ans', result)

instance {-# overlappable #-} (Displayable a, Result a ~ a) => MakeExample a where
    makeExample_ ans i v = do
        result <- (Right <$> evaluate' v)
                 `catch` (\(ex :: SomeException) -> pure (Left ex))
        pure (i, ans, result)

makeExample :: (Displayable (Result t), MakeExample t) => Int -> t -> String -> IO (Maybe String)
makeExample i v name = do
    (_, lhs, result) <- makeExample_ (Result name) i v

    let rhs = either (UhOh . show) id result

    pure $ if hasUnshowableParts rhs
           then Nothing
           else Just (show lhs ++ " = " ++ show rhs)

makeExamples :: (Displayable (Result t), MakeExample t) => [Int] -> t -> String -> IO [String]
makeExamples [] _ _ = pure []
makeExamples (i:is) v name =
    makeExample i v name >>= \case
        Nothing -> makeExamples is v name
        Just e  -> (e:) <$> makeExamples is v name





