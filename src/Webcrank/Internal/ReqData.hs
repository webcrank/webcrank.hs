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
import Prelude

import Webcrank.Internal.Types

-- | Smart constructor for creating a @ReqData@ value with initial values.
newReqData :: ReqData
newReqData = ReqData
  { _reqDataDispPath = []
  , _reqDataRespMediaType = "application" // "octet-stream"
  , _reqDataRespCharset = Nothing
  , _reqDataRespEncoding = Nothing
  , _reqDataRespHeaders = HashMap.empty
  , _reqDataRespBody = Nothing
  }

-- | Lookup a response header.
getResponseHeader
  :: (Functor m, MonadState s m, HasReqData s)
  => HeaderName
  -> m (Maybe ByteString)
getResponseHeader h = (listToMaybe =<<) . HashMap.lookup h <$> use reqDataRespHeaders

-- | Replace any existing response headers for the header name with the
-- new value.
putResponseHeader
  :: (MonadState s m, HasReqData s)
  => HeaderName
  -> ByteString
  -> m ()
putResponseHeader h v = reqDataRespHeaders %= HashMap.insert h [v]

-- | Replace any existing response headers for the header name with the
-- new values.
putResponseHeaders
  :: (MonadState s m, HasReqData s)
  => ResponseHeaders
  -> m ()
putResponseHeaders = mapM_ (uncurry putResponseHeader)

-- | Remove the response header.
removeResponseHeader
  :: (MonadState s m, HasReqData s)
  => HeaderName
  -> m ()
removeResponseHeader h = reqDataRespHeaders %= HashMap.delete h

-- | Lookup the response @Location@ header.
getResponseLocation
  :: (Functor m, MonadState s m, HasReqData s)
  => m (Maybe ByteString)
getResponseLocation = getResponseHeader hLocation

-- | Set the response @Location@ header.
putResponseLocation
  :: (MonadState s m, HasReqData s)
  => ByteString
  -> m ()
putResponseLocation = putResponseHeader hLocation

-- | Use the lazy @ByteString@ as the response body.
writeLBS
  :: (MonadState s m, HasReqData s)
  => LB.ByteString
  -> m ()
writeLBS = (reqDataRespBody ?=)

