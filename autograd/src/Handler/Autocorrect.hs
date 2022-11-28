{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Autocorrect where

import Data.List
import Data.List.Extras.Argmax (argmax)
import qualified Data.Map.Strict as Map
import Data.Set as S (fromList, toList)
import DictionaryDB
import Import hiding (drop, filter, group, head, length, map, null, sort, splitAt, tail, (++), (.))
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck (Arbitrary (..), Gen, Property, Testable (..), (==>))
import qualified Test.QuickCheck as QC
import Prelude hiding (words)

wordsMap :: Handler (Map.Map String Int)
wordsMap = do
  allWords <- runSimDB $ selectList [] []
  let words' = filter (not . null) $ map (toLower . unpack . wordsWord . entityVal) allWords
   in return $ Map.fromList [(head l, length l) | l <- group (sort words')]

-- | Probability of @word@.
p :: Map.Map String Int -> String -> Double
p wm word = (/ n) $ fromIntegral $ fromMaybe 0 (Map.lookup word wm :: Maybe Int)
  where
    n = fromIntegral $ Map.foldl' (+) 0 wm

-- | Most probable spelling correction for @word@.
correction :: Map.Map String Int -> String -> String
correction wm word = argmax (p wm) $ candidates wm word

-- >>> correction "" "sitting"
-- "sitting"

-- an empty map should return the same word
prop_correction :: String -> Bool
prop_correction word = correction Map.empty word == word

noDupCandidates :: [String] -> [String]
noDupCandidates = S.toList . S.fromList

-- | Generate possible spelling corrections for @word@.
candidates :: Map.Map String Int -> String -> [String]
candidates wm word = head $ filter (not . null) s
  where
    s =
      [ known wm [word],
        known wm $ edits1 word,
        known wm $ edits2 word,
        [word]
      ]

-- >>> candidates Map.empty "haskell"
-- ["haskell"]

-- quickcheck for candidates to
prop_candidates :: String -> Bool
prop_candidates word = length (candidates Map.empty word) == 1

-- | The subset of @words'@ that appear in the dictionary of @words@.
known :: Map.Map String Int -> [String] -> [String]
known wm words' = [w | w <- words', Map.member w wm]

-- >>> known Map.empty ["haskell", "python"]
-- []

-- quickcheck for known
prop_known :: String -> Bool
prop_known word = length (known Map.empty [word]) == 0

-- | All edits that are one edit away from @word@.
edits1 :: String -> [String]
edits1 word = deletes ++ transposes ++ replaces ++ inserts
  where
    letters = "abcdefghijklmnopqrstuvwxyz"
    splits = [splitAt i word | i <- [1 .. length word]]
    deletes = [l ++ tail r | (l, r) <- splits, (not . null) r]
    transposes = [l ++ r !! 1 : head r : drop 2 r | (l, r) <- splits, length r > 1]
    replaces = [l ++ c : tail r | (l, r) <- splits, (not . null) r, c <- letters]
    inserts = [l ++ c : r | (l, r) <- splits, c <- letters]

-- quickcheck for edits1
prop_edits1 :: String -> Bool
prop_edits1 word = length (edits1 word) == 26 * (length word + 1) + 25 * (length word - 1)

-- | All edits that are two edits away from @word@.
edits2 :: String -> [String]
edits2 word = [e2 | e1 <- edits1 word, e2 <- edits1 e1]

-- quickcheck for edits2
prop_edits2 :: String -> Bool
prop_edits2 word = length (edits2 word) == 26 * (26 * (length word + 1) + 25 * (length word - 1)) + 25 * (26 * (length word + 1) + 25 * (length word - 1))

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
