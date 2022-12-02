-- | This module is designed for unit tests for Autocorect algorithm
module UnitTests where

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

-- | Test for the most probable spelling correction for @word@.
testCorrection :: Test
testCorrection =
  TestList
    [ correction "speling" ~?= "spelling", --insert
      correction "korrectud" ~?= "corrected", --replace 2
      correction "bycycle" ~?= "bicycle", --replace
      correction "inconvient" ~?= "inconvenient", --insert 2
      correction "arrainged" ~?= "arranged", --delete
      correction "peotry" ~?= "poetry", --transpose
      correction "peotryy" ~?= "poetry", -- transpose + delete
      correction "word" ~?= "word", --known
      correction "quintessential" ~?= "quintessential" --unknown
    ]

-- | Test for probability
testP :: Test
testP =
  TestList
    [ p wm 'quintessential' ~?= 0
    ]

testNoDuplicates :: Test
testNoDuplicates =
  TestList
    [ noDupCandidates ["test", "test", "test1"] ~?= ["test", "test1"]
    ]

testKnown :: Test
testKnown =
  TestList
    [ known Map.empty ["haskell", "python"] ~?= [],
      known Map.fromList ["haskell", "python"] ["haskell", "python"] ~?= ["haskell", "python"]
    ]

testCandidates :: Test
testCandidates =
  TestList
    [ candidates Map.empty "haskell" ~?= ["haskell"]
    ]

testEdits1 :: Test
testEdits1 =
  TestList
    [ length (noDuplicates (edits1 "somthing")) ~?= 390
    ]

testEdits2 :: Test
testEdits2 =
  TestList
    []

runTests :: IO Counts
runTests =
  runTestTT $
    TestList
      [ testCorrection,
        testP,
        testNoDuplicates,
        testKnown,
        testCandidates,
        testEdits1,
        testEdits2
      ]
