module Main where

import qualified AutocorrectTest
import qualified Spec

main :: IO ()
main = do
  putStrLn "*** Testing Autocorrect ***"
  test_all
  qc
  putStrLn "*** Testing Specs ***"
  hspec Spec.spec
