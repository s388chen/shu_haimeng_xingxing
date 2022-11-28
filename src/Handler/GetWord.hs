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

module Handler.GetWord where

import Handler.Autocorrect
import Import

searchForm :: Html -> MForm Handler (FormResult Text, Widget)
searchForm = renderDivs $ areq (searchField True) textSettings Nothing
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
  wm <- wordsMap
  ((formRes, searchWidget), formEnctype) <- runFormGet searchForm
  searchResults <-
    case formRes of
      FormSuccess qstring -> return $ noDupCandidates $ candidates wm (unpack qstring)
      _ -> return []
  defaultLayout $ do
    $(widgetFile "WordRecommendation/word")
    $(widgetFile "WordRecommendation/results")