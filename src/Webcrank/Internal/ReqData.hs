{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Webcrank.Internal.ReqData where

import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import Network.HTTP.Media
import Network.HTTP.Types

import Webcrank.Internal.Types

initReqData :: ServerAPI m -> s -> ReqData s m
initReqData api s = ReqData
  { _reqDataServerAPI = api
  , _reqDataState = s
  , _reqDataDispPath = []
  , _reqDataRespMediaType = "application" // "octet-stream"
  , _reqDataRespCharset = Nothing
  , _reqDataRespEncoding = "identity"
  , _reqDataRespHeaders = Map.empty
  , _reqDataRespBody = Nothing
  }

putResponseMediaType :: MonadState (ReqData s n) m => MediaType -> m ()
putResponseMediaType mt = modify $ \rd -> rd { _reqDataRespMediaType = mt }

putResponseCharset :: MonadState (ReqData s n) m => Maybe Charset -> m ()
putResponseCharset c = modify $ \rd -> rd { _reqDataRespCharset = c }

putResponseEncoding :: MonadState (ReqData s n) m => Encoding -> m ()
putResponseEncoding e = modify $ \rd -> rd { _reqDataRespEncoding = e }

putDispatchPath :: MonadState (ReqData s n) m => [Text] -> m ()
putDispatchPath p = modify $ \rd -> rd { _reqDataDispPath = p }

getResponseHeader :: MonadState (ReqData s n) m => HeaderName -> m (Maybe ByteString)
getResponseHeader h = gets ((listToMaybe =<<) . Map.lookup h . _reqDataRespHeaders)

modifyResponseHeaders
  :: MonadState (ReqData s n) m
  => (Map HeaderName [ByteString] -> Map HeaderName [ByteString])
  -> m ()
modifyResponseHeaders f = modify $ \rd -> rd { _reqDataRespHeaders = f $ _reqDataRespHeaders rd }

putResponseHeader :: MonadState (ReqData s n) m => HeaderName -> ByteString -> m ()
putResponseHeader h v = modifyResponseHeaders (Map.insert h [v])

putResponseHeaders :: MonadState (ReqData s n) m => ResponseHeaders -> m ()
putResponseHeaders = mapM_ (uncurry putResponseHeader)

removeResponseHeader :: MonadState (ReqData s n) m => HeaderName -> m ()
removeResponseHeader h = modifyResponseHeaders $ Map.delete h

getResponseLocation :: MonadState (ReqData s n) m => m (Maybe ByteString)
getResponseLocation = getResponseHeader hLocation

putResponseLocation :: (MonadState (ReqData s n) m) => ByteString -> m ()
putResponseLocation = putResponseHeader hLocation

getResponseBody :: MonadState (ReqData s n) m => m (Maybe LB.ByteString)
getResponseBody = gets _reqDataRespBody

putResponseBody :: MonadState (ReqData s n) m => LB.ByteString -> m ()
putResponseBody b = modify $ \rd -> rd { _reqDataRespBody = Just b }

