{-# LANGUAGE OverloadedStrings #-}
module Webcrank.Conneg 
  ( chooseCharset
  , chooseMediaType
  ) where

import Control.Applicative ((<$>), (<|>))
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.List (find, sort)
import Data.Maybe (listToMaybe)
import Webcrank.Types.MediaType
import Webcrank.Types.Resource

chooseCharset :: [Charset] -> [(Charset, Double)] -> Maybe Charset
chooseCharset = chooseConneg "ISO-8859-1"

chooseConneg :: CI ByteString -> [CI ByteString] -> [(CI ByteString, Double)] -> Maybe (CI ByteString)
chooseConneg def choices range = chooseConneg' def defOk anyOk choices range where
  priority s = snd <$> find ((s ==) . fst) range
  anyOk = maybe False (/= 0.0) (priority "*")
  defOk = maybe anyOk (/= 0.0) (priority def)

chooseConneg' :: CI ByteString             -- default
              -> Bool                      -- default ok
              -> Bool                      -- any ok
              -> [CI ByteString]           -- choices
              -> [(CI ByteString, Double)] -- range
              -> Maybe (CI ByteString)
chooseConneg' _ _ _ [] _ = Nothing
chooseConneg' _ _ True choices [] = listToMaybe choices
chooseConneg' def True False choices [] = find (== def) choices
chooseConneg' _ False False _ [] = Nothing
chooseConneg' def defOk anyOk choices ((h, 0.0) : rest) = loop where
  choices' = filter (/= h) choices
  loop = chooseConneg' def defOk anyOk choices' rest
chooseConneg' def defOk anyOk choices ((h, _) : rest) = match <|> loop where
  match = find (== h) choices
  loop = chooseConneg' def defOk anyOk choices rest
 
-- Determine the @Content-Type@ we will serve for a request.
-- If there is no acceptable/available match, returns @Nothing@.
chooseMediaType :: [MediaType] -- a list of media types the resource can provide.
                -> [MediaType] -- the media types of the request's Accept header, sorted by priority
                -> Maybe MediaType
chooseMediaType _ [] = Nothing
chooseMediaType provided (h:t) = match <|> loop where
  match = mediaMatch h provided
  loop  = chooseMediaType provided t

mediaMatch :: MediaType -> [MediaType] -> Maybe MediaType
mediaMatch _ [] = Nothing
mediaMatch (MediaType "*" "*" []) (h:_) = Just h
mediaMatch (MediaType pri sub ps) provided = listToMaybe [ m | m <- provided, match m ] where
  match (MediaType pri' sub' ps') = typeMatch pri' sub' && paramsMatch ps'
  typeMatch pri' sub' 
    | pri == "*" && sub == "*"   = True
    | pri == pri' && sub == sub' = True
    | otherwise                  = pri == pri' && sub == "*"
  paramsMatch ps' = sort ps == sort ps'

