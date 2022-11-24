{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.GetAutoCorrect where

import Import

getGetAutoCorrectR :: Text -> Handler Html
getGetAutoCorrectR wordId = defaultLayout [whamlet|<h1>Recommendations of term "#{wordId}"|]
