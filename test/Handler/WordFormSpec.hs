{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.WordFormSpec (spec) where

import Foundation
import TestImport

spec :: Spec
spec = withApp $ do
  describe "getWordFormR" $ do
    it "gives a 200 and display the correct form" $ do
      get WordFormR
      statusIs 200
      htmlAnyContain "h1" " Leave your proofreading to us!"
      htmlAnyContain "label" "Simply copy and paste your word into the box below for a easy online spelling check."

  describe "invalid requests" $ do
    it "400s when Post request - Invalid Method" $ do
      request $ do
        setMethod "POST"
        setUrl WordFormR
        addRequestHeader ("Content-Type", "application/json")

      statusIs 405
