{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.FavoritesSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
  describe "Favorites page" $ do
    it "asserts no access to my-account for anonymous users" $ do
      get FavoritesR
      statusIs 303

    it "asserts access to my-account for authenticated users" $ do
      userEntity <- createUser "foo"
      authenticateAs userEntity

      get FavoritesR
      statusIs 200

    it "asserts user's information is shown" $ do
      userEntity <- createUser "bar"
      authenticateAs userEntity

      get FavoritesR
      let (Entity _ user) = userEntity
      htmlAnyContain ".username" . unpack $ userIdent user
