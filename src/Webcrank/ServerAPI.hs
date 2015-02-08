{-# LANGUAGE LambdaCase #-}

module Webcrank.ServerAPI
  ( ServerAPI(..)
  , handleRequest
  ) where

import Control.Applicative
import Control.Monad.Trans.Either
import Data.Foldable (traverse_)
import Network.HTTP.Types

import Webcrank.Internal hiding (respond)

-- TODO
-- - use `MonadCatch m` or `MonadError m` to catch server errors
--   and send a error response?
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

  respondError s _ = -- do
    -- TODO error rendering + make it customizable (pseudo-code follows)
    -- errorHandler = _reqDataErrorHandler d
    -- body <- renderError errorHandler s d
    --
    -- putResponseBody =<< encodeBody body
    return s

  respond s = case statusCode s of
    c | c >= 400 && c < 600 -> respondError s Nothing
    304 ->
      -- TODO
      -- - set etag header
      -- - set expires header
      return s
    _ ->
      -- TODO (in webmachine these are mixed into the decision process)
      -- - set content-type
      -- - set vary
      -- - set body
      --   - if get do encoding, set etag, set last modified, set expires
      return s

  -- TODO
  -- * log decision states
  finish (s, d, _) = do
    srvPutResponseStatus api s
    srvPutResponseHeaders api (_reqDataRespHeaders d)
    traverse_ (srvPutResponseBody api) (_reqDataRespBody d)

