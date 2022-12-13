module Main where

import AutocorrectTest
import Configuration.Dotenv (defaultConfig, loadFile)
import Spec
import Test.Hspec.Runner hiding (defaultConfig)

main :: IO ()
main = do
  putStrLn "*** Testing Specs ***"
  _ <- loadFile defaultConfig
  hspec Spec.spec
  putStrLn "*** Testing Autocorrect ***"
  _ <- test_all
  qc
