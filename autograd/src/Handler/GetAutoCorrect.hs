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

import Conduit
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Data.Text
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Import

getGetAutoCorrectR :: Text -> Handler Html
getGetAutoCorrectR wordId = defaultLayout $ do
  setTitle "GIS Layers"
  runSimDB $ do
    sample <- selectFirst [WordsWlen >. 3] []
    liftIO (print sample)
  [whamlet|<h1>Recommendations of term "#{wordId}"|]
