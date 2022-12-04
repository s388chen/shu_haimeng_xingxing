module AutocorrectTest where

import Data.List
import Data.List.Extras.Argmax (argmax)
import qualified Data.Map.Strict as Map
import Data.Set as S (fromList, toList)
import DictionaryDB
import GHC.Arr
import Handler.Autocorrect
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck (Arbitrary (..), Gen, Property, Testable (..), (==>))
import qualified Test.QuickCheck as QC
import Prelude

-- an empty map should return the same word
prop_correction :: String -> Bool
prop_correction word = correction Map.empty word == word

-- quickcheck for candidates to
prop_candidates :: String -> Bool
prop_candidates word = length (candidates Map.empty word) == 1

-- quickcheck for known
prop_known :: String -> Bool
prop_known word = null (known Map.empty [word])

-- quickcheck for edits1
prop_edits1 :: String -> Bool
prop_edits1 word = length (edits1 word) == 26 * (length word + 1) + 25 * (length word - 1)

-- helper function for checking all elements in the list generated by edits1 is valid
-- calculate the edit distance between two strings
editDistance :: Eq a => [a] -> [a] -> Int
editDistance xs ys = table ! (a, b)
  where
    (a, b) = (length xs, length ys)
    x = array (1, a) (zip [1 ..] xs)
    y = array (1, b) (zip [1 ..] ys)

    table :: Array (Int, Int) Int
    table = array bn [(ij, distance ij) | ij <- range bn]
      where
        bn = ((0, 0), (a, b))

    distance :: (Int, Int) -> Int
    distance (0, j) = j
    distance (i, 0) = i
    distance (i, j) =
      minimum
        [ table ! (i - 1, j) + 1,
          table ! (i, j - 1) + 1,
          if x ! i == y ! j then table ! (i - 1, j - 1) else 1 + table ! (i - 1, j - 1)
        ]

-- quickcheck for all the words generated by edits1 has edit distance 1 from the original word
prop_edits1_distance :: String -> Bool
prop_edits1_distance word = all (\x -> editDistance word x == 1) (edits1 word)

-- this currently fail some cases edits1 currently include the original word in the generated list

-- quickcheck for all the words generated by edits2 has edit distance 2 from the original word
prop_edits2_distance :: String -> Bool
prop_edits2_distance word = all (\x -> editDistance word x <= 3) (edits2 word)

prop_edits2 :: String -> Bool
prop_edits2 word = length (edits2 word) == 26 * (26 * (length word + 1) + 25 * (length word - 1)) + 25 * (26 * (length word + 1) + 25 * (length word - 1))

quickCheckN :: (Testable prop) => Int -> prop -> IO ()
quickCheckN n = QC.quickCheck . QC.withMaxSuccess n

qc :: IO ()
qc = do
  putStrLn "prop_correction"
  quickCheckN 100 prop_correction
  putStrLn "prop_candidates"
  quickCheckN 100 prop_candidates
  putStrLn "prop_known"
  quickCheckN 100 prop_known
  putStrLn "prop_edits1"
  quickCheckN 100 prop_edits1
  putStrLn "prop_edits2"
  quickCheckN 100 prop_edits2
  putStrLn "prop_edits1_distance"
  quickCheckN 100 prop_edits1_distance -- extremely slow
  putStrLn "prop_edits2_distance"
  quickCheckN 100 prop_edits2_distance
