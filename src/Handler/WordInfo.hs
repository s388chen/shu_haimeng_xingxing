{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.WordInfo where

import Import
import Text.Julius (RawJS (..))

-- | Search in dictionary database and return the detailed information of a word
getWordInfoR :: Text -> Handler Html
getWordInfoR wordId =
  do
    wordPs <- runSimDB $ selectList [WordsWord ==. wordId] []
    (userId, _) <- requireAuthPair
    archive <- runDB $ selectList [ArchivedUserId ==. Just userId, ArchivedWord ==. wordId] []
    defaultLayout $
      do
        let (archivedSubmitId, archivedTextId, archivedListId) = archivedIds
        setTitle "Recommendations of term #{wordId}"
        $(widgetFile "WordRecommendation/wordInfo")

-- | Create a few IDs
archivedIds :: (Text, Text, Text)
archivedIds = ("js-archivedSubmit", "js-archivedText", "js-archivedList")