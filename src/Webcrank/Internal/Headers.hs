{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Webcrank.Internal.Headers where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Date
import Network.HTTP.Media
import Network.HTTP.Types

hAcceptCharset, hAcceptEncoding, hAllow, hETag, hExpires, hIfMatch, hIfNoneMatch, hIfUnmodifiedSince, hTransferEncoding, hVary, hWWWAuthenticate :: HeaderName
hAcceptCharset = "Accept-Charset"
hAcceptEncoding = "Accept-Encoding"
hAllow = "Allow"
hETag = "ETag"
hExpires = "Expires"
hIfMatch = "If-Match"
hIfNoneMatch = "If-None-Match"
hIfUnmodifiedSince = "If-Unmodified-Since"
hTransferEncoding = "Transfer-Encoding"
hVary = "Vary"
hWWWAuthenticate = "WWW-Authenticate"

quotedString :: ByteString -> ByteString
quotedString = B.intercalate "\\\"" . B.split 34

instance RenderHeader (CI ByteString) where
  renderHeader = CI.original

instance Accept (CI ByteString) where
  parseAccept = Just . CI.mk
  matches a b = case b of
    "*" -> True
    _ -> a == b
  moreSpecificThan _ b = b == "*"

instance RenderHeader HTTPDate where
  renderHeader = formatHTTPDate
