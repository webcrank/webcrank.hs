{-# LANGUAGE LambdaCase #-}

module Webcrank.ServerAPI
  ( ServerAPI(..)
  , handleRequest
  ) where

import Control.Applicative
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (traverse_)
import Network.HTTP.Date
import Network.HTTP.Types

import Webcrank.Internal hiding (respond)

-- TODO
-- - use `MonadCatch m` or `MonadError m` to catch server errors
--   and send an error response?
handleRequest :: (Applicative m, Monad m)
              => ServerAPI m
              -> Resource s m
              -> m ()
handleRequest api r = initRequest r >>= run >>= finish where
  run = runReqState' (rune <* callr finishRequest) r . initReqData api

  rune = runEitherT (unReqState (runFlowChart b13)) >>= \case
    Left (Error s rs) -> respondError s rs
    Left (Halt s) -> respond s
    Right s -> respond s

  -- TODO
  -- * log decision states
  finish (s, d, _) = do
    srvPutResponseStatus api s
    srvPutResponseHeaders api (_reqDataRespHeaders d)
    traverse_ (srvPutResponseBody api) (_reqDataRespBody d)

respond :: (Applicative m, Monad m) => Status -> ReqState' s m Status
respond s = case statusCode s of
  c | c >= 400 && c < 600 -> respondError s Nothing
  304 -> do
    removeResponseHeader hContentType
    runMaybeT (callr generateETag) >>=
      traverse_ (putResponseHeader hETag . etagBS)
    runMaybeT (callr expires) >>=
      traverse_ (putResponseHeader hExpires . formatHTTPDate)
    return s
  _ ->
    -- TODO (in webmachine these are mixed into the decision process)
    -- - set content-type
    -- - set vary
    -- - if GET or HEAD then set encoding header, set etag, set last modified, set expires, transfer-encoding
    -- - if GET, set body, applying charset and content encoding
    return s

respondError :: (Applicative m, Monad m) => Status -> Maybe LB.ByteString -> ReqState' s m Status
respondError s _ = -- do
  -- TODO error rendering + make it customizable (pseudo-code follows)
  -- errorHandler = _reqDataErrorHandler d
  -- body <- renderError errorHandler s d
  --
  -- putResponseBody =<< encodeBody body
  return s

