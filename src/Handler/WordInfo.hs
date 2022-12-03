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

getWordInfoR :: Text -> Handler Html
getWordInfoR wordId =
  do
    wordPs <- runSimDB $ selectList [WordsWord ==. wordId] []
    defaultLayout $
      do
        setTitle "Recommendations of term #{wordId}"
        $(widgetFile "WordRecommendation/wordInfo")