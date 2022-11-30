{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.GetWordSpec (spec) where

import Foundation
import TestImport

spec :: Spec
spec = withApp $ do
  describe "getGetWordR" $ do
    it "gives a 200 and display the correct form" $ do
      get GetWordR
      statusIs 200
      htmlAnyContain "h1" "Our word recommendation"
      htmlAnyContain "label" "Your term here. You can make a guess."

  describe "invalid requests" $ do
    it "400s when Post request - Invalid Method" $ do
      request $ do
        setMethod "POST"
        setUrl GetWordR
        addRequestHeader ("Content-Type", "application/json")

      statusIs 405
