{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module DictionaryDB where

import qualified ClassyPrelude.Yesod as Control.Monad.Trans.Reader
import Conduit
import qualified Control.Monad.Logger
import Data.Text
import Database.Persist.Sqlite
import Database.Persist.TH
import System.Environment (getEnv)
import Prelude

-- | Create a new model for our dictionary database
share
  [mkPersist sqlSettings]
  [persistLowerCase|
Words sql=words id=(word, type)
  word Text sql=word
  type Text sql=type
  sdex Text sql=sdex
  wlen Int sql=wlen
  defn Text sql=defn
  deriving Show
|]

-- | Run all database records inside the function runSimDB
runSimDB ::
  MonadUnliftIO m =>
  Control.Monad.Trans.Reader.ReaderT SqlBackend (Control.Monad.Logger.NoLoggingT (ResourceT m)) b ->
  m b
runSimDB f = do
  path <- liftIO $ pack <$> getEnv "Dictionary_DB"
  runSqlite path $ do
    f