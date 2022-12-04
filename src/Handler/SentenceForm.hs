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

module Handler.SentenceForm where

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

getSentenceFormR :: Handler Html
getSentenceFormR = do
  wm <- wordsMap
  let ws = Map.keys wm -- wordsSet
  ((formRes, searchWidget), formEnctype) <- runFormGet searchForm
  wrongWords <-
    case formRes of
      FormSuccess qstring -> do
        let lstOfStrs = splitWords (unpack . toLower $ qstring)
        return $ Data.List.filter (`notElem` ws) lstOfStrs
      _ -> return []
  correctedSentence <-
    case formRes of
      FormSuccess qstring -> do
        let lstOfStrs = splitWords (unpack . toLower $ qstring)
        let corrected = concatMap ((++ " ") . (\x -> if x `notElem` ws then correction wm x else x)) lstOfStrs
        return corrected
      _ -> return ""
  defaultLayout $ do
    $(widgetFile "WordRecommendation/sentenceForm")
    -- (widgetFile "WordRecommendation/wrongWords")
    $(widgetFile "WordRecommendation/correctedSentence")

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen pre s = case dropWhile pre s of
  "" -> []
  s' -> w : wordsWhen pre s''
    where
      (w, s'') = break pre s'

splitWords :: String -> [String]
splitWords = wordsWhen (\c -> c == ' ' || c == ',' || c == '.' || c == '"')