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

module Handler.FormWithResults where

import qualified Data.List
import qualified Data.Map as Map
import Handler.Autocorrect
import Import

searchForm :: Html -> MForm Handler (FormResult Text, Widget)
searchForm = renderDivs $ areq (searchField True) textSettings Nothing
  where
    textSettings =
      FieldSettings
        { fsLabel = "Simply copy and paste your text into the box below for a easy online spelling check.",
          fsTooltip = Nothing,
          fsId = Nothing,
          fsName = Nothing,
          fsAttrs =
            [ ("class", "form-control"),
              ("placeholder", "Please enter your text here")
            ]
        }

getFormWithResultsR :: Handler Html
getFormWithResultsR = do
  wm <- wordsMap
  let ws = Map.keys wm -- wordsSet
  ((formRes, searchWidget), formEnctype) <- runFormGet searchForm
  searchResults <-
    case formRes of
      FormSuccess qstring -> do
        let lstOfStrs = wordsWhen (== ' ') (unpack . toLower $ qstring)
        case Data.List.find (`notElem` ws) lstOfStrs of
          Just wrongWord -> return $ noDupCandidates $ candidates wm wrongWord
          Nothing -> return []
      _ -> return []
  defaultLayout $ do
    $(widgetFile "WordRecommendation/searchForm")
    $(widgetFile "WordRecommendation/results")

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen pre s = case dropWhile pre s of
  "" -> []
  s' -> w : wordsWhen pre s''
    where
      (w, s'') = break pre s'
