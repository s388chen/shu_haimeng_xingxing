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

module DictionaryDB where

import Conduit
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Data.Text
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import System.Environment (getEnv)
import Prelude

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

runSimDB f = do
  path <- liftIO $ pack <$> getEnv "Dictionary_DB"
  runSqlite path $ do
    f