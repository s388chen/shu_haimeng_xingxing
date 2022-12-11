{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Home where

import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
getHomeR :: Handler Html
getHomeR = do
  let handlerName = "getHomeR" :: Text

  defaultLayout $ do
    aDomId <- newIdent
    setTitle "Welcome To Autocorrection!"
    $(widgetFile "homepage")