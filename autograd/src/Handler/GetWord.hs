{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.GetWord where

import Import
import Yesod.Form.Bootstrap3

newtype Term = Term
    {term :: Text}

searchForm :: AForm Handler Term
searchForm = Term <$> areq textField (bfs ("Term" :: Text)) Nothing

getGetWordR :: Handler Html
getGetWordR = do
    (formWidget, formEnctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm searchForm
    defaultLayout $ do
        $(widgetFile "WordRecommendation/word")