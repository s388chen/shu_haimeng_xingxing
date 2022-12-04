{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Autocorrect where

import Data.List
import Data.List.Extras.Argmax (argmax)
import qualified Data.Map.Strict as Map
import Data.Set as S (fromList, toList)
import DictionaryDB
import Import
  ( Entity (entityVal),
    Handler,
    PersistStoreWrite (delete),
    Textual (toLower),
    fromMaybe,
    selectList,
    unpack,
  )
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

-- >>> Data.Map.lookup "a" wordsMap2
-- Just 53

-- | Probability of @word@.
p :: Map.Map String Int -> String -> Double
p wm word = (/ n) $ fromIntegral $ fromMaybe 0 (Map.lookup word wm :: Maybe Int)
  where
    n = fromIntegral $ Map.foldl' (+) 0 wm

-- | Most probable spelling correction for @word@.
correction :: Map.Map String Int -> String -> String
correction wm word = argmax (p wm) $ candidates wm word

-- >>> correction Map.empty "sitting"
-- "sitting"

-- >>> candidates (Map.fromList[("corrected", 1)]) "korrectud"
-- ["corrected","corrected"]

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
edits1 :: String -> [String]
edits1 word = deletes ++ replaces ++ inserts
  where
    alphabet = ['a' .. 'z']
    splits = [splitAt i word | i <- [0 .. length word]]
    deletes = [l ++ tail r | (l, r) <- splits, (not . null) r]
    replaces = [l ++ c : tail r | (l, r) <- splits, (not . null) r, c <- alphabet]
    inserts = [l ++ c : r | (l, r) <- splits, c <- alphabet]

-- >>> edits1 ""
-- ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]

-- | All possible edits that are two edits away from @word@.
edits2 :: String -> [String]
edits2 word = [e2 | e1 <- edits1 word, e2 <- edits1 e1]
