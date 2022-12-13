{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.ArchiveSpec (spec) where

import Data.Aeson
import Import
import TestImport

spec :: Spec
spec = withApp $ do
  describe "Get ArchiveSpec" $ do
    it "Get ArchiveSpec" $ do
      TestImport.get ArchiveR
      statusIs 405

  describe "Post ArchiveSpec" $ do
    it "Post ArchiveSpec - No login" $ do
      let body = object ["foo" .= ("My message" :: Value)]
      request $ do
        setMethod "POST"
        setUrl ArchiveR
        setRequestBody $ encode body
        addRequestHeader ("Content-Type", "application/json")

      statusIs 400
    it "Post ArchiveSpec - Authorized" $ do
      userEntity <- createUser "bar"
      authenticateAs userEntity

      let body = object ["word" .= ("test" :: Value)]
      request $ do
        setMethod "POST"
        setUrl ArchiveR
        setRequestBody $ encode body
        addRequestHeader ("Content-Type", "application/json")

      statusIs 200

    it "Post ArchiveSpec - 400 Bad Request" $ do
      userEntity <- createUser "bar"
      authenticateAs userEntity

      let body = object ["bar" .= ("test" :: Value)]
      request $ do
        setMethod "POST"
        setUrl ArchiveR
        setRequestBody $ encode body
        addRequestHeader ("Content-Type", "application/json")

      statusIs 400
