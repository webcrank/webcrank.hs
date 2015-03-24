{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Webcrank.Internal.ReqData where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Network.HTTP.Media
import Network.HTTP.Types

import Webcrank.Internal.Types

newReqData :: ReqData
newReqData = ReqData
  { _reqDataDispPath = []
  , _reqDataRespMediaType = "application" // "octet-stream"
  , _reqDataRespCharset = Nothing
  , _reqDataRespEncoding = Nothing
  , _reqDataRespHeaders = HashMap.empty
  , _reqDataRespBody = Nothing
  }

getResponseHeader
  :: (Functor m, MonadState s m, HasReqData s)
  => HeaderName
  -> m (Maybe ByteString)
getResponseHeader h = (listToMaybe =<<) . HashMap.lookup h <$> use reqDataRespHeaders

putResponseHeader
  :: (MonadState s m, HasReqData s)
  => HeaderName
  -> ByteString
  -> m ()
putResponseHeader h v = reqDataRespHeaders %= HashMap.insert h [v]

putResponseHeaders
  :: (MonadState s m, HasReqData s)
  => ResponseHeaders
  -> m ()
putResponseHeaders = mapM_ (uncurry putResponseHeader)

removeResponseHeader
  :: (MonadState s m, HasReqData s)
  => HeaderName
  -> m ()
removeResponseHeader h = reqDataRespHeaders %= HashMap.delete h

getResponseLocation
  :: (Functor m, MonadState s m, HasReqData s)
  => m (Maybe ByteString)
getResponseLocation = getResponseHeader hLocation

putResponseLocation
  :: (MonadState s m, HasReqData s)
  => ByteString
  -> m ()
putResponseLocation = putResponseHeader hLocation

writeLBS
  :: (MonadState s m, HasReqData s)
  => LB.ByteString
  -> m ()
writeLBS = (reqDataRespBody ?=)

