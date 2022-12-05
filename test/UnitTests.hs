-- | This module is designed for unit tests for Autocorect algorithm
module UnitTests where

import Data.List
import qualified Data.Map.Strict as Map
import Handler.Autocorrect
  ( candidates,
    correction,
    edits1,
    edits2,
    known,
    noDupCandidates,
    p,
  )
import Test.HUnit (Counts, Test (..), runTestTT, (~?=))
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
      correction testDict3 "peotryy" ~?= "poetry", -- transpose + delete
      correction testDict3 "word" ~?= "word", -- known
      correction testDict3 "quintessential" ~?= "quintessential", -- unknown
      correction testDict3 "eptf" ~?= "word" -- shift1
    ]

-- | Test for probability
testP :: Test
testP =
  TestList
    [ p testDict1 "quintessential" ~?= 0
    ]

testNoDupCandidates :: Test
testNoDupCandidates =
  TestList
    [ noDupCandidates ["test", "test", "test1"] ~?= ["test", "test1"]
    ]

testKnown :: Test
testKnown =
  TestList
    [ known testDict1 [] ~?= [],
      known testDict1 ["haskell", "python"] ~?= ["haskell", "python"]
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
      length (edits1 "somthing") ~?= 450,
      length (noDupCandidates (edits1 "somthing")) ~?= 435
    ]

testEdits2 :: Test
testEdits2 =
  TestList
    [ length (edits2 "s") ~?= 8944,
      length (edits2 "ss") ~?= 21452,
      length (edits2 "sa") ~?= 21452,
      length (noDupCandidates (edits2 "ss")) ~?= 6505,
      length (noDupCandidates (edits2 "sa")) ~?= 7130
    ]

runTests :: IO Counts
runTests =
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
