module Main where

import AutocorrectTest
import Spec
import Test.Hspec.Runner

main :: IO ()
main = do
  putStrLn "*** Testing Autocorrect ***"
  _ <- test_all
  qc
  putStrLn "*** Testing Specs ***"
  hspec Spec.spec
