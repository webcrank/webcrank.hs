{-# LANGUAGE OverloadedStrings #-}
module Webcrank.Types.MediaType where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI

data MediaType = MediaType (CI ByteString) (CI ByteString) [(CI ByteString, ByteString)]
  deriving Eq

instance Show MediaType where
  show = show . renderMediaType

renderMediaType :: MediaType -> ByteString
renderMediaType (MediaType pri sub ps) = B.concat [CI.original pri, "/", CI.original sub, params] where
  params = B.concat [ B.concat [";", CI.original k, "=\"", B.concatMap escape v, "\""] | (k, v) <- ps ]
  escape '\\' = "\\\\"
  escape '"' = "\\\""
  escape c = B.singleton c

textHtml :: MediaType
textHtml = MediaType "text" "html" []

applicationXml :: MediaType
applicationXml = MediaType "application" "xml" []

textXml :: MediaType
textXml = MediaType "text" "xml" []

applicationJson :: MediaType
applicationJson = MediaType "application" "json" []

textJson :: MediaType
textJson = MediaType "text" "json" []

imageJpeg :: MediaType
imageJpeg = MediaType "image" "jpeg" []
