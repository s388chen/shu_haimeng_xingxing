import Data.Array
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck (Arbitrary (..), Gen, Property, Testable (..), (==>))
import qualified Test.QuickCheck as QC

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

-- >>> editDistance "kitten" "sitting"
-- 3

-- >>> editDistance "hello" "Hello"
-- 1

-- add quickcheck for editDistance
-- first quickcheck for The edit distance between any string and itself is 0.
prop_editdistSelf :: [Char] -> Bool
prop_editdistSelf xs = editDistance xs xs == 0

-- second quickcheck for The edit distance between any string and the empty string is the length of the string.
prop_editdistEmpty :: [Char] -> Bool
prop_editdistEmpty xs = editDistance xs [] == length xs

-- third quickcheck for the edit distance from x to y should be the same as from y to x. (symmetry)
prop_editdistReverse :: [Char] -> [Char] -> Bool
prop_editdistReverse xs ys = editDistance xs ys == editDistance ys xs

-- fourth quickcheck for After applying n edits to a string x, the distance between the edited string and x should be at most n.
prop_editdistMax :: [Char] -> Int -> Bool
prop_editdistMax xs n = editDistance xs (take n xs) <= n

-- fifth quickcheck for No two strings have a negative edit distance.
prop_editdistNegative :: [Char] -> [Char] -> Bool
prop_editdistNegative xs ys = editDistance xs ys >= 0

qc :: IO ()
qc = do
  putStrLn "prop_editdistSelf"
  quickCheckN 100 prop_editdistSelf
  putStrLn "prop_editdistEmpty"
  quickCheckN 100 prop_editdistEmpty
  putStrLn "prop_editdistReverse"
  quickCheckN 100 prop_editdistReverse
  putStrLn "prop_editdistMax"
  quickCheckN 100 prop_editdistMax
  putStrLn "prop_editdistNegative"
  quickCheckN 100 prop_editdistNegative
