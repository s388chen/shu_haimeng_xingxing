{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.GetWord where

import Import

newtype Term = Term
  {term :: Text}

searchForm :: AForm Handler Term
searchForm = Term <$> areq textField textSettings Nothing
  where
    textSettings =
      FieldSettings
        { fsLabel = "Your term here. You can make a guess.",
          fsTooltip = Nothing,
          fsId = Nothing,
          fsName = Nothing,
          fsAttrs =
            [ ("class", "form-control"),
              ("placeholder", "Please enter your term")
            ]
        }

getGetWordR :: Handler Html
getGetWordR = do
  (formWidget, formEnctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm searchForm
  defaultLayout $ do
    $(widgetFile "WordRecommendation/word")