{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.GetAutoCorrectSpec (spec) where

import DictionaryDB
import Handler.Autocorrect
import Handler.GetAutoCorrect
import TestImport

spec :: Spec
spec = withApp $ do
  describe "getGetAutoCorrectR" $ do
    it "gives a 200 and return matched results" $ do
      let wordId = "test" :: Text
      get (GetAutoCorrectR wordId)
      statusIs 200

      matched <- runSimDB $ selectList [WordsWord ==. wordId] []
      assertEq "Should have " 2 $ length matched

    it "gives a 200 and return no match" $ do
      let wordId = "" :: Text
      get (GetAutoCorrectR wordId)
      statusIs 200

      matched <- runSimDB $ selectList [WordsWord ==. wordId] []
      assertEq "Should have " 0 $ length matched
