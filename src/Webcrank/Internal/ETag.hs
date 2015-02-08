{-# LANGUAGE OverloadedStrings #-}

module Webcrank.Internal.ETag where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (parseOnly, string)
import Data.ByteString (ByteString)
import Data.Monoid

import Webcrank.Internal.Types
import Webcrank.Internal.Parsers

strongComparison :: ETag -> ETag -> Bool
strongComparison e1 e2 = case (e1, e2) of
  (StrongETag v1, StrongETag v2) -> v1 == v2
  _ -> False

weakComparison :: ETag -> ETag -> Bool
weakComparison e1 e2 = opaqueTag e1 == opaqueTag e2

opaqueTag :: ETag -> ByteString
opaqueTag e = case e of StrongETag v -> v; WeakETag v -> v

etagBS :: ETag -> ByteString
etagBS e = case e of
  StrongETag t -> t
  WeakETag t -> "W/" <> t

parseETags :: ByteString -> [ETag]
parseETags = either (const[]) id . parseOnly (csl1 etagP) where
  etagP = weakP <|> strongP
  weakP = WeakETag <$> (string "W/" *> quotedStringP)
  strongP = StrongETag <$> quotedStringP
