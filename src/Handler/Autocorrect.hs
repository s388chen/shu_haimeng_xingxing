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
        known wm $ transpose1 word,
        known wm $ shift1 word,
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

-- | All words after one possible transpose
-- we do not combine this with edits1 because it is actually edit2 but this happen more frequently than common edit2 cases
transpose1 :: String -> [String]
transpose1 word = transposes
  where
    splits = [splitAt i word | i <- [0 .. length word]]
    transposes = [l ++ r !! 1 : head r : drop 2 r | (l, r) <- splits, length r > 1]

-- >>> transpose1 "haskell"
-- ["ahskell","hsakell","haksell","hasekll","hasklel","haskell"]

-- | All possible edits that are two edits away from @word@.
edits2 :: String -> [String]
edits2 word = [e3 ++ e4 | e1 <- edits1 word, e2 <- transpose1 word, e3 <- edits1 e1, e4 <- edits1 e2]

-- >>> length (noDupCandidates (edits1 "somthing"))
-- 435

-- if our hand shift one letter left on the keyboard, we may input a word with a large edit distance
-- but actually this is a common mistake
shift1left :: String -> String
shift1left word = [shift1' c | c <- word]
  where
    shift1' :: Char -> Char
    shift1' c
      | c == 'q' = 'w'
      | c == 'w' = 'e'
      | c == 'e' = 'r'
      | c == 'r' = 't'
      | c == 't' = 'y'
      | c == 'y' = 'u'
      | c == 'u' = 'i'
      | c == 'i' = 'o'
      | c == 'o' = 'p'
      | c == 'p' = 'a'
      | c == 'a' = 's'
      | c == 's' = 'd'
      | c == 'd' = 'f'
      | c == 'f' = 'g'
      | c == 'g' = 'h'
      | c == 'h' = 'j'
      | c == 'j' = 'k'
      | c == 'k' = 'l'
      | c == 'l' = 'z'
      | c == 'z' = 'x'
      | c == 'x' = 'c'
      | c == 'c' = 'v'
      | c == 'v' = 'b'
      | c == 'b' = 'n'
      | c == 'n' = 'm'
      | c == 'm' = 'q'
      | otherwise = c

-- >>> shift1 "gwkki"
-- "hello"

shift1right :: String -> String
shift1right word = [shift1' c | c <- word]
  where
    shift1' :: Char -> Char
    shift1' c
      | c == 'q' = 'm'
      | c == 'w' = 'q'
      | c == 'e' = 'w'
      | c == 'r' = 'e'
      | c == 't' = 'r'
      | c == 'y' = 't'
      | c == 'u' = 'y'
      | c == 'i' = 'u'
      | c == 'o' = 'i'
      | c == 'p' = 'o'
      | c == 'a' = 'p'
      | c == 's' = 'a'
      | c == 'd' = 's'
      | c == 'f' = 'd'
      | c == 'g' = 'f'
      | c == 'h' = 'g'
      | c == 'j' = 'h'
      | c == 'k' = 'j'
      | c == 'l' = 'k'
      | c == 'z' = 'l'
      | c == 'x' = 'z'
      | c == 'c' = 'x'
      | c == 'v' = 'c'
      | c == 'b' = 'v'
      | c == 'n' = 'b'
      | c == 'm' = 'n'
      | otherwise = c

-- >>> shift1right "hello"
-- "gwkki"

-- >>> shift1right "hppf"
-- "good"

-- combine the result of shift1left and shift1right to a list
shift1 :: String -> [String]
shift1 word = [shift1left word, shift1right word]

-- >>> shift1 "hppf"
-- ["jaag","good"]
