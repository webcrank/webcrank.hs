{-# LANGUAGE OverloadedStrings #-}
module Webcrank.Conneg 
  ( chooseMediaType
  ) where

import Control.Applicative ((<|>))
import Data.List (sort)
import Data.Maybe (listToMaybe)
import Webcrank.Types.MediaType

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
mediaMatch (MediaType pri sub ps) provided = listToMaybe [ m' | m' <- provided, match m' ] where
  match (MediaType pri' sub' ps') = typeMatch pri' sub' && paramsMatch ps'
  typeMatch _ _       | pri == "*" && sub == "*"   = True
  typeMatch pri' sub' | pri == pri' && sub == sub' = True
                      | otherwise                  = sub == "*" && pri == pri'
  paramsMatch ps' = sort ps == sort ps'

