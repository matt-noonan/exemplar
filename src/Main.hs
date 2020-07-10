{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Exemplar
import           Exemplar.TH
import           Language.Haskell.TH

-- $(monomorphize 'foldr)
-- $(monomorphize 'uncurry)
-- $(monomorphize 'reverse)

{-
6/6/20: (<*>) doesn't turn up any examples, no [a -> b] or Maybe (a -> b)
6/7/20:
  - concat is showing a bunch of duplicates, e.g.
          concat [] = ""
          concat ["Hello", "world!"] = "Helloworld!"
          concat [] = ""
          concat ["Hello", "world!"] = "Helloworld!"
  - Many functions seem to hang, e.g. $(exemplarsOf '(&))
  - ^ these were actually ok, just generating WAAAY too many examples.
    Added a limit to throttle the number of valid example-generating
    type assignments.
  - $(exemplarsOf 'maybe) now fails?
  - $(exemplarsOf 'maybeToList) doesn't give great results (all are of the
    form "maybeToList Nothing = []")
6/8/20:
  - maybeToList seems to be fixed
  - maybe still fails
  - words and unwords generate no examples via TH, even though makeExamples is fine.
  - (FakeIO a) should be un-Showy *except* for when it appears at the outermost
    level, since we can actually run the computation in that case.
  - When the throttle is enabled and has a low threshold (e.g. findAllExamples 5),
    $(exemplarsOf 'pure) returns some fairly complicated examples.
  - pure @(Either Int) @(Down Int) isn't properly formatting its output.
6/10/20:
  - $(exemplarsOf 'foldr) tries to make an exemplar for (Char -> String -> String),
    but there is no instance for that type. Why is it trying to do that?!
    ARGH! this only happens if findExampleVariables uses filterRepeats, instead of
    Set.toList . Set.fromList. WTF.
    NOTE: actually it happens with Set.toList . Set.fromList too, but it is
    hidden by the throttle in `forLimit`.
    foldr :: (a -> b -> b) -> [a] -> b -> b
    The problematic "example" is coming from the constraint instance part of
    `exampleInstances`.
-}
main :: IO ()
main = pure ()
