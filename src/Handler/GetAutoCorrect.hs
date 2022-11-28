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

module Handler.GetAutoCorrect where

import Control.Monad.IO.Class (liftIO)
import Import

getGetAutoCorrectR :: Text -> Handler Html
getGetAutoCorrectR wordId =
  do
    wordPs <- runSimDB $ selectList [WordsWord ==. wordId] [] --to be tested
    -- liftIO (print words)
    defaultLayout $
      do
        setTitle "Recommendations of term #{wordId}"
        $(widgetFile "WordRecommendation/words")
