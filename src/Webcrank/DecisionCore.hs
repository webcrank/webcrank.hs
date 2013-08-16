{-# LANGUAGE OverloadedStrings #-}

module Webcrank.DecisionCore 
  ( runResource
  , Response
  ) where

import Control.Applicative ((<$>))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (runStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.ByteString.Lazy.Builder (byteString, charUtf8)
import Data.ByteString.Lazy.Builder.ASCII (intDec)
import Data.Monoid
import Network.HTTP.Types
import Webcrank.Internal
import Webcrank.Types

data Step = V3B13 | V3B12

type Response rb = (Status, ResponseHeaders, ResponseBody rb)

type ResFn rq rb s m a = ReaderT (Resource rq rb m s) (ResourceFn rq rb s m) a

-- TODO add more configuration
--   * error handler
--   * app base
--   * tracing
runResource :: (Functor m, Monad m, HasRequestInfo rq) => Resource rq rb m s -> rq -> m (Response rb)
runResource r rq = runMaybeT (rqInit r) >>= maybe serverError go where
  -- TODO use error handler to get body
  serverError = return (internalServerError500, [], BuilderResponseBody $ byteString "500 Internal server error") 
  go s = runFn (decision V3B13) (initRqData rq s)
  runFn f s = mkResp <$> runStateT (runResourceFn (runReaderT f r)) s
  mkResp (ResourceFnValue resp, _) = resp
  mkResp (ResourceFnError e, rqd) = (internalServerError500, respHdrs rqd, e)
  -- TODO use error handler to get body for 400 <= s < 600, handle 304 (remove content-type header, generate Etag, expires headers)
  mkResp (ResourceFnHalt s, rqd) = (s, respHdrs rqd, BuilderResponseBody $ byteString "")

-- TODO tracing
step :: (Monad m, HasRequestInfo rq) => Step -> ResFn rq rb s m (Response rb)
step = decision

test :: (Monad m, Eq a) => ResFn rq rb s m a        -- function to test
                        -> a                        -- expected test value
                        -> ResFn rq rb s m (Response rb) -- true step
                        -> ResFn rq rb s m (Response rb) -- false step
                        -> ResFn rq rb s m (Response rb)
test r x t f = r >>= \y -> if x == y then t else f

call :: (Resource rq rb m s -> ResourceFn rq rb s m a) -> ResFn rq rb s m a
call = ReaderT

respond :: Monad m => Status -> ResFn rq rb s m (Response rb)
-- TODO pull headers out of RqData
respond s = return (s, [], BuilderResponseBody $ byteString "")

errorResponse :: Monad m => Status -> ResFn rq rb s m (Response rb)
errorResponse s = lift $ do
  b <- getErrorRenderer >>= ($ s)
  hdrs <- getRespHeaders
  return (s, hdrs, b)

decision :: (Monad m, HasRequestInfo rq) => Step -> ResFn rq rb s m (Response rb)

-- Service Available
decision V3B13 = test (call serviceAvailable) True (step V3B12) (errorResponse serviceUnavailable503)

decision _ = Prelude.error "step not implemented"

defaultErrorRenderer :: Monad m => ErrorRenderer rq rb s m
defaultErrorRenderer s = getRespBody >>= maybe (render s) return where
  render (Status 404 _) = do
    addRespHeader hContentType "text/html"
    return $ BuilderResponseBody $ byteString "<html><head><title>404 Not Found</title></head><body><h1>Not Found</h1>The requested document was not found on this server.<p><hr><address>webcrank web server</address></body></html>"
  render (Status 503 _) = do
    addRespHeader hContentType "text/html"
    return $ BuilderResponseBody $ byteString "<html><head><title>503 Service Unavailable</title></head><body><h1>Service Unavailable</h1>The server is currently unable to handle the request due to a temporary overloading or maintenance of the server.<br><p><hr><address>webcrank web server</address></body></html>"
  render (Status c msg) = do
    addRespHeader hContentType "text/html"
    return $ BuilderResponseBody $ mconcat [ byteString "<html><head><title>" 
                                           , intDec c
                                           , charUtf8 ' '
                                           , byteString msg
                                           , byteString "</title></head><body><h1>"
                                           , byteString msg
                                           , byteString "</h1>The server encountered an error while processing this request.<p><hr><address>webcrank web server</address></body></html>"
                                           ]

initRqData :: Monad m => rq -> s -> RqData rq rb s m
initRqData rq s = RqData
  { rqInfo = rq
  , rqState = s
  , errorRenderer = defaultErrorRenderer
  , respHdrs = []
  , respBody = Nothing
  }

