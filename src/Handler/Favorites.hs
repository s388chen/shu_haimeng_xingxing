{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Favorites where

import Import

getFavoritesR :: Handler Html
getFavoritesR = do
  (userId, user) <- requireAuthPair
  allWords <- runDB $ selectList [ArchivedUserId ==. Just userId] []
  defaultLayout $ do
    setTitle . toHtml $ userIdent user <> "'s User page"
    $(widgetFile "profile")