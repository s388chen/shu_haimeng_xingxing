{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Favorites where

import Import

-- | Start off by defining a Result datatype
data Result = Result
  { resultTitle :: Text,
    resultExcerpt :: Text
  }

-- | Return the results in a user's favorites
getFavoritesR :: Handler Html
getFavoritesR = do
  (userId, user) <- requireAuthPair
  allWords <- runDB $ selectList [ArchivedUserId ==. Just userId] []
  archives <- runSimDB $
    forM allWords $ \can -> do
      x <- selectFirst [WordsWord ==. archivedWord (entityVal can)] []
      case x of
        Just entity ->
          return $
            Result {resultTitle = wordsWord $ entityVal entity, resultExcerpt = wordsDefn $ entityVal entity}
        Nothing -> return Result {resultTitle = archivedWord $ entityVal can, resultExcerpt = ""}
  defaultLayout $ do
    setTitle . toHtml $ userIdent user <> "'s User page"
    $(widgetFile "favorites")