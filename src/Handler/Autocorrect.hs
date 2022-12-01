{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Autocorrect where

import Data.Char (isAlpha)
import Data.List
import Data.List.Extras.Argmax (argmax)
import qualified Data.Map.Strict as Map
import Data.Set as S (fromList, toList, union)
import DictionaryDB
import GHC.IO
import Import hiding (drop, filter, group, head, length, map, null, sort, splitAt, tail, (++), (.))
import Prelude hiding (words)

wordsMap :: Handler (Map.Map String Int)
wordsMap = do
  allWords <- runSimDB $ selectList [] []
  let words' = filter (not . null) $ map (toLower . unpack . wordsWord . entityVal) allWords
   in return $ Map.fromList [(head l, length l) | l <- group (sort words')]

-- wordsMap2 :: Map.Map String Int
-- {-# NOINLINE wordsMap #-}
-- wordsMap2 = do
--   Map.fromList [(head l, length l) | l <- group (sort words')]
--   where
--     text = unsafePerformIO $ Prelude.readFile "big.txt"
--     words' = filter (not . null) . splitWhen (not . isAlpha) $ map Data.Char.toLower text

-- >>> Data.Map.lookup "a" wordsMap
-- Just 53

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

-- | The subset of @words'@ that appear in the dictionary of @words@.
known :: Map.Map String Int -> [String] -> [String]
known wm words' = [w | w <- words', Map.member w wm]

-- >>> known Map.empty ["haskell", "python"]
-- []

-- | All edits that are one edit away from @word@.
-- | For a word of length n, there will be n deletions, n-1 transpositions,
-- | 26n alterations, and 26(n+1) insertions,
-- | for a total of 54n+25 (with duplicates removed)
edits1 :: String -> [String]
edits1 word = S.toList (deletes `S.union` transposes `S.union` replaces `S.union` inserts)
  where
    letters = "abcdefghijklmnopqrstuvwxyz"
    splits = [splitAt i word | i <- [1 .. length word]]
    deletes = S.fromList [l ++ tail r | (l, r) <- splits, (not . null) r]
    transposes = S.fromList [l ++ r !! 1 : head r : drop 2 r | (l, r) <- splits, length r > 1]
    replaces = S.fromList [l ++ c : tail r | (l, r) <- splits, (not . null) r, c <- letters]
    inserts = S.fromList [l ++ c : r | (l, r) <- splits, c <- letters]

-- >>> length (edits1 "somthing")
-- 442

-- | All edits that are two edits away from @word@.
edits2 :: String -> [String]
edits2 word = [e2 | e1 <- edits1 word, e2 <- edits1 e1]
