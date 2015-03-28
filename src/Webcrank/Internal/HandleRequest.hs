{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Webcrank.Internal.HandleRequest where

import qualified Blaze.ByteString.Builder as BB
import qualified Blaze.ByteString.Builder.Char.Utf8 as BB
import Control.Applicative
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Foldable (traverse_)
import Network.HTTP.Media
import Network.HTTP.Types

import Webcrank.Internal.DecisionCore
import Webcrank.Internal.Halt
import Webcrank.Internal.Headers
import Webcrank.Internal.Types
import Webcrank.Internal.ReqData
import Webcrank.Internal.ResourceData

-- | Process a request according to the webmachine state diagram. Intended for
-- use by server API providers. @run@ is a function which can run process to
-- completion.
--
-- @'Webcrank.ServerAPI.WebcrankT'@ is provided as a starting point. For the type
--
-- @
-- type WaiCrank = ReaderT (Request, HTTPDate) (WebcrankT IO)
-- @
--
-- an appropriate @run@ function would be
--
-- @
-- run :: Resource WaiCrank -> Request -> HTTPDate -> WaiCrank a -> IO (a, ReqData, LogData)
-- run resource req date wa = runReaderT (runWebcrankT wa (ResourceData api resource) newReqData) (req, date)
-- @
handleRequest
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s, MonadCatch m, Functor n)
  => (forall a. m a -> n (a, ReqData, LogData)) -- ^ run
  -> n (Status, HeadersMap, Maybe Body)
handleRequest run = run handler <&> finish where
  handler = (decisionCore <* callr'' finishRequest) `catch` handleError
  decisionCore = runHaltT (runFlowChart b13) >>= \case
    Left (Error s rs) -> s <$ prepError s rs
    Left (Halt s) -> s <$ prepResponse s
    Right s -> s <$ prepResponse s

  -- TODO log decision states
  finish (s, d, _) =
    (s, _reqDataRespHeaders d, _reqDataRespBody d)

prepResponse
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => Status
  -> m ()
prepResponse s = case statusCode s of
  c | c >= 400 && c < 600 -> prepError s (LB.fromStrict $ statusMessage s)
  304 -> do
    removeResponseHeader hContentType
    reqDataRespBody .= Nothing
    let header h rm = traverse_ (putResponseHeader h . renderHeader) =<< callr'' (runMaybeT . rm)
    header hETag generateETag
    header hExpires expires
  _ -> return ()

-- TODO make it customizable
prepError
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => Status
  -> LB.ByteString
  -> m ()
prepError s r = assign reqDataRespBody . Just =<< encodeBody' =<< renderError s r

handleError
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => SomeException
  -> m Status
handleError = (internalServerError500 <$) . prepError internalServerError500 . LB.fromString . show

renderError
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => Status
  -> LB.ByteString
  -> m Body
renderError s reason = maybe (render s reason) return =<< use reqDataRespBody where

render
  :: (Functor m, HasReqData s, HasResourceData r m, MonadReader r m, MonadState s m)
  => Status
  -> LB.ByteString
  -> m LB.ByteString
render s reason =  putResponseHeader hContentType "text/html" >> (errorBody s reason)

errorBody
  :: (Functor m, HasResourceData r m, MonadReader r m)
  => Status
  -> LB.ByteString
  -> m LB.ByteString
errorBody s reason = case statusCode s of
    404 -> return "<html><head><title>404 Not Found</title></head><body><h1>Not Found</h1>The requested document was not found on this server.<p><hr><address>webcrank web server</address></body></html>"
    500 -> return $ mconcat
      [ "<html><head><title>500 Internal Server Error</title></head><body><h1>Internal Server Error</h1>The server encountered an error while processing this request:<br><pre>"
      , reason
      , "</pre><p><hr><address>webcrank web server</address></body></html>"
      ]
    501 -> getRequestMethod' <&> \m -> mconcat
      [ "<html><head><title>501 Not Implemented</title></head><body><h1>Not Implemented</h1>The server does not support the "
      , LB.fromStrict m
      , " method.<br><p><hr><address>webmachine web server</address></body></html>"
      ]
    503 -> return "<html><head><title>503 Service Unavailable</title></head><body><h1>Service Unavailable</h1>The server is currently unable to handle the request due to a temporary overloading or maintenance of the server.<br><p><hr><address>webcrank web server</address></body></html>"
    _ -> return $ BB.toLazyByteString $ mconcat
      [ BB.fromByteString "<html><head><title>"
      , BB.fromShow $ statusCode s
      , BB.fromByteString " "
      , BB.fromByteString $ statusMessage s
      , BB.fromByteString "</title></head><body><h1>"
      , BB.fromByteString $ statusMessage s
      , BB.fromByteString "</h2>"
      , BB.fromLazyByteString reason
      , BB.fromByteString "<p><hr><address>webcrank web server</address></body></html>"
      ]

