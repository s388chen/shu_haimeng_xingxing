{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.FormWithResultsSpec (spec) where

import Foundation
import TestImport

spec :: Spec
spec = withApp $ do
  describe "getFormWithResultsR" $ do
    it "gives a 200 and display the correct form" $ do
      get FormWithResultsR
      statusIs 200
      htmlAnyContain "h1" "Our word recommendation"
      htmlAnyContain "label" "Your term here. You can make a guess."

  describe "invalid requests" $ do
    it "400s when Post request - Invalid Method" $ do
      request $ do
        setMethod "POST"
        setUrl FormWithResultsR
        addRequestHeader ("Content-Type", "application/json")

      statusIs 405
