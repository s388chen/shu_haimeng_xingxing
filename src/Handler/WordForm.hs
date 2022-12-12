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

module Handler.WordForm where

import Handler.Autocorrect
import Import

data Result = Result
  { resultTitle :: Text,
    resultExcerpt :: Text
  }

searchForm :: Html -> MForm Handler (FormResult Text, Widget)
searchForm = renderDivs $ areq (searchField True) textSettings Nothing
  where
    textSettings =
      FieldSettings
        { fsLabel = "Simply copy and paste your word into the box below for a easy online spelling check.",
          fsTooltip = Nothing,
          fsId = Nothing,
          fsName = Nothing,
          fsAttrs =
            [ ("class", "form-control"),
              ("placeholder", "Please enter your text here")
            ]
        }

getWordFormR :: Handler Html
getWordFormR = do
  wm <- wordsMap
  ((formRes, searchWidget), formEnctype) <- runFormGet searchForm
  searchResults <-
    case formRes of
      FormSuccess qstring ->
        runSimDB $
          forM (noDupCandidates (candidates wm (unpack . toLower $ qstring))) $ \can -> do
            x <- selectFirst [WordsWord ==. pack can] []
            case x of
              Just entity ->
                return $
                  Result {resultTitle = wordsWord $ entityVal entity, resultExcerpt = wordsDefn $ entityVal entity}
              Nothing -> return Result {resultTitle = pack can, resultExcerpt = ""}
      _ -> return []
  defaultLayout $ do
    $(widgetFile "WordRecommendation/wordForm")
    $(widgetFile "WordRecommendation/results")