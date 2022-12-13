{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Home where

import Import

-- Home page
getHomeR :: Handler Html
getHomeR = do
  let handlerName = "getHomeR" :: Text

  defaultLayout $ do
    aDomId <- newIdent
    setTitle "Welcome To Autocorrection!"
    $(widgetFile "homepage")