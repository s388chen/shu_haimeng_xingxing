{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Lib

main :: IO ()
main = do
  putStrLn someFunc
  putStrLn "Test suite not yet implemented"