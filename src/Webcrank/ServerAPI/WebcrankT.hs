{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Webcrank.ServerAPI.WebcrankT where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.RWS
import Prelude

import Webcrank.Internal.Types

-- | Monad transformer that can be used by server API providers.
-- Provides tracking of the request state and logging of the
-- decisions made so far. For example
--
-- @
-- type WaiCrank m a = ReaderT (Request, HTTPDate) (WebcrankT m) a
-- @
--
newtype WebcrankT m a =
  WebcrankT { unWebcrankT :: RWST (ResourceData (WebcrankT m)) LogData ReqData m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (ResourceData (WebcrankT m))
    , MonadState ReqData
    , MonadWriter LogData
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

instance MonadTrans WebcrankT where
  lift = WebcrankT . lift

runWebcrankT
  :: WebcrankT m a
  -> ServerAPI (WebcrankT m)
  -> Resource (WebcrankT m)
  -> ReqData
  -> m (a, ReqData, LogData)
runWebcrankT w s r rd = runRWST (unWebcrankT w) (ResourceData s r) rd

