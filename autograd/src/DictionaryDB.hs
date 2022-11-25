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
import Prelude

share
  [mkPersist sqlSettings]
  [persistLowerCase|
Words sql=words id=(word, type)
  word String sql=word
  type String sql=type
  sdex String sql=sdex
  wlen Int sql=wlen
  defn String sql=defn
  deriving Show
|]

pravasConf =
  SqliteConf
    { sqlDatabase = "/Users/irenekxx/Desktop/mcit/552/shu_haimeng_xingxing/autograd/config/Dictionary.db",
      sqlPoolSize = 2
    }

runSimDB f = runSqlite (sqlDatabase pravasConf) $ do
  f