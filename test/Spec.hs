{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Lib
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn someFunc
  putStrLn "Test suite not yet implemented"