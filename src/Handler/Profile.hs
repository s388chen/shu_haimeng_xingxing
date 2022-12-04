{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Profile where

import Import

getProfileR :: Handler Html
getProfileR = do
  (userId, user) <- requireAuthPair
  allWords <- runDB $ selectList [ArchivedUserId ==. Just userId] []
  defaultLayout $ do
    setTitle . toHtml $ userIdent user <> "'s User page"
    $(widgetFile "profile")