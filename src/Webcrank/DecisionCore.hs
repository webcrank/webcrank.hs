{-# LANGUAGE OverloadedStrings #-}

module Webcrank.DecisionCore 
  ( runResource
  , Response
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy.Builder (Builder, byteString, charUtf8)
import Data.ByteString.Lazy.Builder.ASCII (intDec)
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid
import qualified Data.List.NonEmpty as NEL
import Network.HTTP.Types
import Webcrank.Conneg
import Webcrank.Parsers
import Webcrank.Types.Internal
import Webcrank.Types.MediaType
import Webcrank.Types.Resource

data Step = V3B3 | V3C3
          | V3B4 | V3C4 | V3D4
          | V3B5        | V3D5 | V3E5
          | V3B6               | V3E6 ByteString | V3F6
          | V3B7
          | V3B8
          | V3B9
          | V3B10
          | V3B11
          | V3B12
          | V3B13

type Response rb = (Status, ResponseHeaders, Maybe (ResponseBody rb))

newtype ResFn rq rb s m a = ResFn { unResFn :: ReaderT (Resource rq rb m s) (ResourceFn rq rb s m) (Result a) }

instance Monad m => Functor (ResFn rq rb s m) where
  fmap = liftM

instance Monad m => Applicative (ResFn rq rb s m) where
  pure = return
  (<*>) = ap

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
runResource :: (Monad m, HasRequestInfo rq) => Resource rq rb m s -> rq -> m (Response rb)
runResource r rq = runInit (rqInit r) >>= go where
  go s = runFn (decision V3B13) (initRqData rq s)
  runFn = evalStateT . unResourceFn . (resultToResp =<<) . runResFn r

runResFn :: Resource rq rb m s -> ResFn rq rb s m a -> ResourceFn rq rb s m (Result a)
runResFn = flip (runReaderT . unResFn)

resultToResp :: Monad m => Result (Response rb) -> ResourceFn rq rb s m (Response rb)
resultToResp (Value resp) = return resp
resultToResp (Error e) = errorResponse internalServerError500 e
resultToResp (Halt s) = mkResp s Nothing

errorResponse :: Monad m => Status -> Builder -> ResourceFn rq rb s m (Response rb)
errorResponse s e = mkResp s (Just e)

-- TODO handle 304 (remove content-type header, generate Etag, expires headers)
mkResp :: Monad m => Status -> Maybe Builder -> ResourceFn rq rb s m (Response rb)
mkResp s e = do
  b <- getRespBody >>= \b -> case b of
    Nothing | statusCode s >= 400 && statusCode s < 600 -> getErrorRenderer >>= ($ (s, e))
    _ -> return b
  hdrs <- getRespHeaders
  return (s, hdrs, b)

-- TODO tracing
step :: (Monad m, HasRequestInfo rq) => Step -> ResFn rq rb s m (Response rb)
step = decision

v3d4 :: (Monad m, HasRequestInfo rq) => MediaType -> ResFn rq rb s m (Response rb)
v3d4 = (>> step V3D4) . call . putRespMediaType

v3f6 :: (Monad m, HasRequestInfo rq) => Maybe Charset -> ResFn rq rb s m (Response rb)
v3f6 = (>> step V3F6) . call . putRespCharset

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

callr' :: Monad m => (Resource rq rb m s -> ResourceFn rq rb s m a) -> ResFn rq rb s m a
callr' = ResFn . liftM return . ReaderT 

call :: Monad m => ResourceFn rq rb s m a -> ResFn rq rb s m a
call = ResFn . lift . liftM return

respond :: Monad m => Status -> ResFn rq rb s m a
respond = ResFn . return . Halt

-- TODO make it part of the config or part of the resource?
knownMethods :: [Method]
knownMethods = [methodGet, methodHead, methodPost, methodPut, methodDelete, methodTrace, methodConnect, methodOptions]

decision :: (Monad m, HasRequestInfo rq) => Step -> ResFn rq rb s m (Response rb)

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
    else call (putRespHeader "Allow" (allowHeader ms)) >> respond methodNotAllowed405
  where allowHeader = B.intercalate ", "

-- Malformed?
decision V3B9 = testEq (callr malformedRequest) True (respond badRequest400) (step V3B8)

-- Authorized?
decision V3B8 = do
  authz <- callr isAuthorized
  case authz of
    Authorized -> step V3B7
    (Unauthorized h) -> call (putRespHeader "WWW-Authenticate" h) >> respond unauthorized401

-- Forbidden?
decision V3B7 = testEq (callr forbidden) True (respond forbidden403) (step V3B6)

-- Okay Content-* Headers?
decision V3B6 = testEq (callr validContentHeaders) True (step V3B5) (respond notImplemented501)

-- Known Content-Type?
decision V3B5 = testEq (callr knownContentType) True (step V3B4) (respond unsupportedMediaType415)

-- Req Entity Too Large?
decision V3B4 = testEq (callr validEntityLength) True (step V3B3) (respond requestEntityTooLarge413)

-- OPTIONS?
decision V3B3 = do
  m <- call getRqMethod
  if m == methodOptions
    then do 
      hs <- callr' options 
      call (putRespHeaders hs)
      respond ok200
    else step V3C3

-- Accept exists?
decision V3C3 = do
  accept <- call $ getRqHeader hAccept
  cs <- callr' contentTypesProvided 
  let def = v3d4 $ fst $ NEL.head cs
      v3c4 = const $ step V3C4
  maybe def v3c4 accept

-- Acceptable media type available?
decision V3C4 = do
  ctypes <- callr' contentTypesProvided
  accept <- call $ getRqHeader hAccept
  let mtypes = fst <$> NEL.toList ctypes
      choice = accept >>= chooseMediaType mtypes . parseAccept
  maybe (call $ errorResponse notAcceptable406 (byteString "No acceptable media type available")) v3d4 choice

-- Accept-Language exists?
decision V3D4 = test (call $ getRqHeader hAcceptLanguage) isNothing (step V3E5) (step V3D5)

-- Acceptable Language available?
-- TODO implement proper conneg
decision V3D5 = testEq (return True) True (step V3E5) (respond notAcceptable406)

-- Accept-Charset exists?
decision V3E5 = call (getRqHeader "Accept-Charset") >>= choose where
  choose = maybe (chooseProvidedCharset "*" >>= v3f6) (step . V3E6)

-- Acceptable Charset available?
decision (V3E6 h) = chooseProvidedCharset h >>= next where
  next = maybe (call $ errorResponse notAcceptable406 (byteString "No acceptable charset available")) (v3f6 . Just)

decision _ = Prelude.error "step not implemented"

chooseProvidedCharset :: Monad m => ByteString -> ResFn rq rb s m (Maybe Charset)
chooseProvidedCharset acc = choose <$> callr' charsetsProvided where
  choose NoCharset = Nothing
  choose (CharsetsProvided cs) = chooseCharset (fst <$> NEL.toList cs) (parseAcceptLang acc)

defaultErrorRenderer :: (Monad m, HasRequestInfo rq) => ErrorRenderer rq rb s m
defaultErrorRenderer (s, e) = liftM Just (render s) where
  render (Status 404 _) = do
    addRespHeader hContentType "text/html"
    return $ BuilderResponseBody $ byteString "<html><head><title>404 Not Found</title></head><body><h1>Not Found</h1>The requested document was not found on this server.<p><hr><address>webcrank web server</address></body></html>"
  render (Status 501 _) = do
    addRespHeader hContentType "text/html"
    m <- getRqMethod
    return $ BuilderResponseBody $ mconcat [ byteString "<html><head><title>501 Not Implemented</title></head><body><h1>Not Implemented</h1>The server does not support the "
                                           , byteString m
                                           , byteString " method.<p><hr><address>webcrank web server</address></body></html>"
                                           ]
  render (Status 503 _) = do
    addRespHeader hContentType "text/html"
    return $ BuilderResponseBody $ byteString "<html><head><title>503 Service Unavailable</title></head><body><h1>Service Unavailable</h1>The server is currently unable to handle the request due to a temporary overloading or maintenance of the server.<p><hr><address>webcrank web server</address></body></html>"
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
  { rqInfo        = rq
  , rqState       = s
  , errorRenderer = defaultErrorRenderer
  , respMediaType = MediaType "application" "octet-stream" []
  , respCharset   = Nothing
  , respHdrs      = []
  , respBody      = Nothing
  }

