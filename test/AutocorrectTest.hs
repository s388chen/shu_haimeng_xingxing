module AutocorrectTest where

import Data.List
import Data.List.Extras.Argmax (argmax)
import qualified Data.Map.Strict as Map
import Data.Set as S (fromList, toList)
import DictionaryDB
import Handler.Autocorrect
import Handler.GetAutoCorrect
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

-- quickcheck for edits2
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
