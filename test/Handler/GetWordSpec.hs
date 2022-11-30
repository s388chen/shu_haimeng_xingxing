{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.GetWordSpec (spec) where

import Foundation
import Handler.GetWord
import TestImport

spec :: Spec
spec = withApp $ do
  describe "getGetWordR" $ do
    it "gives a 200" $ do
      get GetWordR
      statusIs 200

  describe "invalid requests" $ do
    it "400s when Post request - Invalid Method" $ do
      request $ do
        setMethod "POST"
        setUrl GetWordR
        addRequestHeader ("Content-Type", "application/json")

      statusIs 405