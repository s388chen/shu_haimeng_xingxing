module AutocorrectTest where

import Data.List
import qualified Data.Map.Strict as Map
import GHC.Arr
import Handler.Autocorrect
import Test.HUnit (Counts, Test (..), runTestTT, (~?=))
import Test.QuickCheck (Testable (..))
import qualified Test.QuickCheck as QC
import Prelude

testDict1 :: Map.Map String Int
testDict1 =
  Map.fromList
    [ ("haskell", 5),
      ("python", 2),
      ("java", 3)
    ]

testDict2 :: Map.Map String Int
testDict2 = Map.empty

testDict3 :: Map.Map String Int
testDict3 =
  Map.fromList
    [ ("haskell", 5),
      ("python", 2),
      ("java", 3),
      ("spelling", 1),
      ("corrected", 10),
      ("bicycle", 1),
      ("inconvenient", 1),
      ("arranged", 1),
      ("poetry", 1),
      ("word", 1)
    ]

-- | Test for the most probable spelling correction for @word@.
testCorrection :: Test
testCorrection =
  TestList
    [ correction testDict3 "speling" ~?= "spelling", -- insert
      correction testDict3 "korrectud" ~?= "corrected", -- replace 2
      correction testDict3 "bycycle" ~?= "bicycle", -- replace
      correction testDict3 "inconvient" ~?= "inconvenient", -- insert 2
      correction testDict3 "arrainged" ~?= "arranged", -- delete
      correction testDict3 "peotry" ~?= "poetry", -- transpose
      correction testDict3 "word" ~?= "word", -- known
      correction testDict3 "quintessential" ~?= "quintessential", -- unknown
      correction testDict3 "haskel" ~?= "haskell", -- delete
      correction testDict3 "pythn" ~?= "python", -- delete
      correction testDict3 "jav" ~?= "java", -- delete
      correction testDict3 "speling" ~?= "spelling", -- insert
      correction testDict3 "correcpud" ~?= "corrected", -- replace 2
      correction testDict3 "bycicle" ~?= "bicycle", -- replace
      correction testDict3 "wrd" ~?= "word", -- delete
      correction testDict3 "eptf" ~?= "word", -- shift1
      correction testDict3 "ksbs" ~?= "java" -- shift1
    ]

-- | Test for probability
testP :: Test
testP =
  TestList
    [ p testDict1 "quintessential" ~?= 0,
      p testDict1 "haskell" ~?= 0.5,
      p testDict1 "python" ~?= 0.2,
      p testDict1 "java" ~?= 0.3,
      p testDict3 "quintessential" ~?= 0,
      p testDict3 "haskell" ~?= 0.19230769230769232,
      p testDict3 "python" ~?= 7.692307692307693e-2,
      p testDict3 "java" ~?= 0.11538461538461539,
      p testDict3 "spelling" ~?= 3.8461538461538464e-2,
      p testDict3 "corrected" ~?= 0.38461538461538464,
      p testDict3 "bicycle" ~?= 3.8461538461538464e-2,
      p testDict3 "inconvenient" ~?= 3.8461538461538464e-2,
      p testDict3 "arranged" ~?= 3.8461538461538464e-2,
      p testDict3 "poetry" ~?= 3.8461538461538464e-2,
      p testDict3 "word" ~?= 3.8461538461538464e-2
    ]

testNoDupCandidates :: Test
testNoDupCandidates =
  TestList
    [ noDupCandidates ["test", "test", "test1"] ~?= ["test", "test1"],
      noDupCandidates ["test", "test", "test1", "test1"] ~?= ["test", "test1"],
      noDupCandidates ["test", "test", "test1", "test1", "test2"] ~?= ["test", "test1", "test2"],
      noDupCandidates ["test", "test", "test1", "test1", "test2", "test2"] ~?= ["test", "test1", "test2"],
      noDupCandidates ["test", "test", "test1", "test1", "test2", "test2", "test3"] ~?= ["test", "test1", "test2", "test3"],
      noDupCandidates ["test", "test", "test1", "test1", "test2", "test2", "test3", "test3"] ~?= ["test", "test1", "test2", "test3"],
      noDupCandidates ["test", "test", "test1", "test1", "test2", "test2", "test3", "test3", "test4"] ~?= ["test", "test1", "test2", "test3", "test4"],
      noDupCandidates ["test", "test", "test1", "test1", "test2", "test2", "test3", "test3", "test4", "test4"] ~?= ["test", "test1", "test2", "test3", "test4"],
      noDupCandidates ["test", "test", "test1", "test1", "test2", "test2", "test3", "test3", "test4", "test4", "test5"] ~?= ["test", "test1", "test2", "test3", "test4", "test5"],
      noDupCandidates ["test", "test", "test1", "test1", "test2", "test2", "test3", "test3", "test4", "test4", "test5", "test5"] ~?= ["test", "test1", "test2", "test3", "test4", "test5"],
      noDupCandidates ["test", "test", "test1", "test1", "test2", "test2", "test3", "test3", "test4", "test4", "test5", "test5", "test6"] ~?= ["test", "test1", "test2", "test3", "test4", "test5", "test6"],
      noDupCandidates ["test", "test", "test1", "test1", "test2", "test2", "test3", "test3", "test4", "test4", "test5", "test5", "test6", "test6"] ~?= ["test", "test1", "test2", "test3", "test4", "test5", "test6"],
      noDupCandidates ["test", "test", "test1", "test1", "test2", "test2", "test3", "test3", "test4", "test4", "test5", "test5", "test6", "test6", "test7"] ~?= ["test", "test1", "test2", "test3", "test4", "test5", "test6", "test7"],
      noDupCandidates ["test", "test", "test1", "test1", "test2", "test2", "test3", "test3", "test4", "test4", "test5", "test5", "test6", "test6", "test7", "test7"] ~?= ["test", "test1", "test2", "test3", "test4", "test5", "test6", "test7"],
      noDupCandidates ["test", "test", "test1", "test1", "test2", "test2", "test3", "test3", "test4", "test4", "test5", "test5", "test6", "test6", "test7", "test7", "test8"] ~?= ["test", "test1", "test2", "test3", "test4", "test5", "test6", "test7", "test8"],
      noDupCandidates ["test", "test", "test1", "test1", "test2", "test2", "test3", "test3", "test4", "test4", "test5", "test5", "test6", "test6", "test7", "test7", "test8", "test8"] ~?= ["test", "test1", "test2", "test3", "test4", "test5", "test6", "test7", "test8"]
    ]

testKnown :: Test
testKnown =
  TestList
    [ known testDict1 [] ~?= [],
      known testDict1 ["haskell", "python"] ~?= ["haskell", "python"],
      known testDict1 ["poetry", "word"] ~?= [],
      known testDict2 ["haskell", "python"] ~?= [],
      known testDict2 ["poetry", "word"] ~?= [],
      known testDict3 ["poetry", "word"] ~?= ["poetry", "word"],
      known testDict3 ["inconvenient", "hello"] ~?= ["inconvenient"]
    ]

testCandidates :: Test
testCandidates =
  TestList
    [ candidates testDict1 "haskel" ~?= ["haskell", "haskell"],
      candidates testDict2 "haskell" ~?= ["haskell"],
      candidates testDict3 "haskell" ~?= ["haskell"]
    ]

testEdits1 :: Test
testEdits1 =
  TestList
    [ length (edits1 "") ~?= 26,
      length (edits1 "a") ~?= 79,
      length (edits1 "ab") ~?= 132,
      length (edits1 "abc") ~?= 185,
      length (edits1 "abcd") ~?= 238,
      length (edits1 "aaaa") ~?= 238,
      length (edits1 "somthing") ~?= 450,
      length (noDupCandidates (edits1 "")) ~?= 26,
      length (noDupCandidates (edits1 "s")) ~?= 78,
      length (noDupCandidates (edits1 "ab")) ~?= 129,
      length (noDupCandidates (edits1 "aa")) ~?= 128,
      length (noDupCandidates (edits1 "abc")) ~?= 180,
      length (noDupCandidates (edits1 "abcd")) ~?= 231,
      length (noDupCandidates (edits1 "somthing")) ~?= 435
    ]

testEdits2 :: Test
testEdits2 =
  TestList
    [ length (edits2 "a") ~?= 8944,
      length (edits2 "s") ~?= 8944,
      length (edits2 "ss") ~?= 21452,
      length (edits2 "sa") ~?= 21452,
      length (edits2 "abc") ~?= 39578,
      length (edits2 "aaa") ~?= 39578,
      length (edits2 "abcd") ~?= 63322,
      length (edits2 "aaaa") ~?= 63322,
      length (edits2 "something") ~?= 266312,
      length (noDupCandidates (edits2 "s")) ~?= 2654,
      length (noDupCandidates (edits2 "ss")) ~?= 6505,
      length (noDupCandidates (edits2 "sa")) ~?= 7130,
      length (noDupCandidates (edits2 "abc")) ~?= 14206,
      length (noDupCandidates (edits2 "aaa")) ~?= 12230,
      length (noDupCandidates (edits2 "aba")) ~?= 14204,
      length (noDupCandidates (edits2 "abcd")) ~?= 23883,
      length (noDupCandidates (edits2 "qqqq")) ~?= 19830,
      length (noDupCandidates (edits2 "aaaa")) ~?= 19830,
      length (noDupCandidates (edits2 "something")) ~?= 111283
    ]

-- an empty map should return the same word
prop_correction :: String -> Bool
prop_correction word = correction Map.empty word == word

-- quickcheck for candidates
-- if the word is not in the dictionary, the length of the list should be 1
prop_candidates :: String -> Bool
prop_candidates word = length (candidates Map.empty word) == 1

-- quickcheck for known
prop_known :: String -> Bool
prop_known word = null (known Map.empty [word])

-- quickcheck for edits1
prop_edits1 :: String -> Bool
prop_edits1 word = length (edits1 word) == 26 * (length word * 2 + 1) + length word

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

-- helper function
-- apply the above editDistance function to all elements in a list and return a list of integers
-- if the list is empty, return an empty list
editDistanceList :: Eq a => [[a]] -> [a] -> [Int]
editDistanceList [] _ = []
editDistanceList (x : xs) word = editDistance x word : editDistanceList xs word

-- quickcheck for all the words generated by edits1 has edit distance 1 from the original word
-- we use <= not == because the original word is also included in our edit1 list
prop_edits1_distance :: String -> Bool
prop_edits1_distance word = all (\x -> editDistance word x <= 1) (edits1 word)

-- quickcheck for all the words generated by edits2 has edit distance 2 from the original word
prop_edits2_distance :: String -> Bool
prop_edits2_distance word = all (\x -> editDistance word x <= 2) (edits2 word)

quickCheckN :: (Testable prop) => Int -> prop -> IO ()
quickCheckN n = QC.quickCheck . QC.withMaxSuccess n

-------------------------- all properties and tests in this module  -----------------------------
test_all :: IO Counts
test_all =
  runTestTT $
    TestList
      [ testCorrection,
        testP,
        testNoDupCandidates,
        testKnown,
        testCandidates,
        testEdits1,
        testEdits2
      ]

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
  putStrLn "prop_edits1_distance"
  quickCheckN 100 prop_edits1_distance -- extremely slow, around 10 min
  putStrLn "prop_edits2_distance"
  quickCheckN 100 prop_edits2_distance -- around 80 min, only for testing
