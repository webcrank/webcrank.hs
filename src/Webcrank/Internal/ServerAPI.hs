{-# LANGUAGE FlexibleContexts #-}

module Webcrank.Internal.ServerAPI
  ( getRequestMethod
  , getRequestHeader
  , getRequestTime
  , getRequestURI
  ) where

import Control.Monad.State
import Data.ByteString (ByteString)
import Network.HTTP.Date
import Network.HTTP.Types

import Webcrank.Internal.Types

callAPI :: (MonadState (ReqData s m) (t m), MonadTrans t, Monad m) => (ServerAPI m -> m b) -> t m b
callAPI f = get >>= lift . f . _reqDataServerAPI

getRequestMethod :: (MonadState (ReqData s m) (t m), MonadTrans t, Monad m) => t m Method
getRequestMethod = callAPI srvGetRequestMethod

getRequestHeader :: (MonadState (ReqData s m) (t m), MonadTrans t, Monad m) => HeaderName -> t m  (Maybe ByteString)
getRequestHeader = callAPI . flip srvGetRequestHeader

getRequestTime :: (MonadState (ReqData s m) (t m), MonadTrans t, Monad m) => t m HTTPDate
getRequestTime = callAPI srvGetRequestTime

getRequestURI :: (MonadState (ReqData s m) (t m), MonadTrans t, Monad m) => t m ByteString
getRequestURI = callAPI srvGetRequestURI

