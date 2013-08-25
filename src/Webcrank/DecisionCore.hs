{-# LANGUAGE OverloadedStrings #-}

module Webcrank.DecisionCore 
  ( runResource
  , Response
  ) where

import Control.Monad (liftM)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Class
import qualified Data.ByteString as B
import Data.ByteString.Lazy.Builder (byteString, charUtf8)
import Data.ByteString.Lazy.Builder.ASCII (intDec)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Network.HTTP.Types
import Webcrank.Internal
import Webcrank.Types

data Step = V3B3
          | V3B4
          | V3B5
          | V3B6
          | V3B7
          | V3B8
          | V3B9
          | V3B10
          | V3B11
          | V3B12
          | V3B13

type Response rb = (Status, ResponseHeaders, ResponseBody rb)

newtype ResFn rq rb s m a = ResFn { unResFn :: ReaderT (Resource rq rb m s) (ResourceFn rq rb s m) (Result a) }

instance Monad m => Monad (ResFn rq rb s m) where
  return = ResFn . return . return
  f >>= g = ResFn $ unResFn f >>= g' where
    g' (Value a) = unResFn $ g a
    g' (Error e) = return $ Error e
    g' (Halt s) = return $ Halt s

instance MonadTrans (ResFn rq rb s) where
  lift m = ResFn $ lift $ lift $ liftM return m

-- TODO add more configuration
--   * error handler
--   * app base
--   * tracing
runResource :: (Functor m, Monad m, HasRequestInfo rq) => Resource rq rb m s -> rq -> m (Response rb)
runResource r rq = runInit (rqInit r) >>= go where
  go s = runFn (decision V3B13) (initRqData rq s)
  runFn f = evalStateT (unResourceFn (runReaderT (unResFn f) r >>= runFn'))
  runFn' (Value resp) = return resp
  runFn' (Error e) = do
    b <- getErrorRenderer >>= ($ (internalServerError500, Just e))
    hdrs <- getRespHeaders
    return (internalServerError500, hdrs, b)
  -- TODO handle 304 (remove content-type header, generate Etag, expires headers)
  runFn' (Halt s) = do
    b <- getErrorRenderer >>= ($ (s, Nothing))
    hdrs <- getRespHeaders
    return (s, hdrs, b)

-- TODO tracing
step :: (Functor m, Monad m, HasRequestInfo rq) => Step -> ResFn rq rb s m (Response rb)
step = decision

test :: Monad m => ResFn rq rb s m a        -- function to test
                -> (a -> Bool)              -- test function
                -> ResFn rq rb s m (Response rb) -- true step
                -> ResFn rq rb s m (Response rb) -- false step
                -> ResFn rq rb s m (Response rb)
test r f x y = r >>= \a -> if f a then x else y

testEq :: (Monad m, Eq a) => ResFn rq rb s m a        -- function to test
                          -> a                        -- test value
                          -> ResFn rq rb s m (Response rb) -- true step
                          -> ResFn rq rb s m (Response rb) -- false step
                          -> ResFn rq rb s m (Response rb)
testEq r a x y = r >>= \b -> if a == b then x else y

callr :: (Resource rq rb m s -> ResourceFn rq rb s m (Result a)) -> ResFn rq rb s m a
callr = ResFn . ReaderT

callr' :: Functor m => (Resource rq rb m s -> ResourceFn rq rb s m a) -> ResFn rq rb s m a
callr' = ResFn . fmap return . ReaderT 

call :: (Functor m, Monad m) => ResourceFn rq rb s m a -> ResFn rq rb s m a
call = ResFn . lift . fmap return

respond :: Monad m => Status -> ResFn rq rb s m a
respond = ResFn . return . Halt

-- TODO make it part of the config or part of the resource?
knownMethods :: [Method]
knownMethods = [methodGet, methodHead, methodPost, methodPut, methodDelete, methodTrace, methodConnect, methodOptions]

decision :: (Functor m, Monad m, HasRequestInfo rq) => Step -> ResFn rq rb s m (Response rb)

-- Service Available
decision V3B13 = testEq (callr serviceAvailable) True (step V3B12) (respond serviceUnavailable503)

-- Known method?
decision V3B12 = test (call getRqMethod) known (step V3B11) (respond notImplemented501) where 
  known m = m `elem` knownMethods

-- URI too long?
decision V3B11 = testEq (callr uriTooLong) True (respond requestURITooLong414) (step V3B10) 

-- Method allowed?
decision V3B10 = do
  ms <- callr' allowedMethods
  m  <- call getRqMethod
  if m `elem` ms
    then step V3B9
    else call (setRespHeader "Allow" (allowHeader ms)) >> respond methodNotAllowed405
  where allowHeader = B.intercalate ", "

-- Malformed?
decision V3B9 = testEq (callr malformedRequest) True (respond badRequest400) (step V3B8)

-- Authorized?
decision V3B8 = do
  authz <- callr isAuthorized
  case authz of
    Authorized -> step V3B7
    (Unauthorized h) -> call (setRespHeader "WWW-Authenticate" h) >> respond unauthorized401

-- Forbidden?
decision V3B7 = testEq (callr forbidden) True (respond forbidden403) (step V3B6)

-- Okay Content-* Headers?
decision V3B6 = testEq (callr validContentHeaders) True (step V3B5) (respond notImplemented501)

-- Known Content-Type?
decision V3B5 = testEq (callr knownContentType) True (step V3B4) (respond unsupportedMediaType415)

-- Req Entity Too Large?
decision V3B4 = testEq (callr validEntityLength) True (step V3B3) (respond requestEntityTooLarge413)

decision _ = Prelude.error "step not implemented"

defaultErrorRenderer :: (Monad m, HasRequestInfo rq) => ErrorRenderer rq rb s m
defaultErrorRenderer (s, e) = getRespBody >>= maybe (render s) return where
  render (Status 404 _) = do
    addRespHeader hContentType "text/html"
    return $ BuilderResponseBody $ byteString "<html><head><title>404 Not Found</title></head><body><h1>Not Found</h1>The requested document was not found on this server.<p><hr><address>webcrank web server</address></body></html>"
  render (Status 501 _) = do
    addRespHeader hContentType "text/html"
    m <- getRqMethod
    return $ BuilderResponseBody $ mconcat [ byteString "<html><head><title>501 Not Implemented</title></head><body><h1>Not Implemented</h1>The server does not support the "
                                           , byteString m
                                           , byteString " method.<br><p><hr><address>webcrank web server</address></body></html>"
                                           ]
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
                                           , byteString "</h1>"
                                           , fromMaybe (byteString msg) e
                                           , byteString "<p><hr><address>webcrank web server</address></body></html>"
                                           ]

initRqData :: (Monad m, HasRequestInfo rq) => rq -> s -> RqData rq rb s m
initRqData rq s = RqData
  { rqInfo = rq
  , rqState = s
  , errorRenderer = defaultErrorRenderer
  , respHdrs = []
  , respBody = Nothing
  }

