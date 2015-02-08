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
import Data.ByteString.Lazy.Builder (Builder, byteString, charUtf8)
import Data.ByteString.Lazy.Builder.ASCII (intDec)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Network.HTTP.Types
import Webcrank.Types.Internal
import Webcrank.Types.MediaType
import Webcrank.Types.Resource

data Step = V3B13

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
  let sc = statusCode s
  b <- getRespBody >>= \b -> case b of
    Nothing | sc >= 400 && sc < 600 -> getErrorRenderer >>= ($ (s, e))
    _ -> return b
  hdrs <- getRespHeaders
  return (s, hdrs, b)


decision :: (Monad m, HasRequestInfo rq) => Step -> ResFn rq rb s m (Response rb)
decision = undefined

-- setContentType :: Monad m => ResFn rq rb s m ()
-- setContentType = call (ctype >>= putRespHeader hContentType) where
--   ctype = (<>) <$> mtype <*> cset
--   mtype = renderMediaType <$> getRespMediaType
--   cset = maybe "" (("; charset=" <>) . CI.original) <$> getRespCharset

-- setVary :: Monad m => ResFn rq rb s m ()
-- setVary = vars >>= set where
--   set [] = return ()
--   set vs = call (putRespHeader "Vary" $ B.intercalate ", " vs)
--   vars = vary "Accept" contentTypesProvided NEL.toList
--      <@> vary "Accept-Encoding" encodingsProvided id
--      <@> vary "Accept-Charset" charsetsProvided toCharsetList
--      <@> ((CI.original <$>) <$> callr' variances)
--   vary h f g = vary' . length . g <$> callr' f where
--     vary' l | l < 2     = []
--             | otherwise = [h]
--   toCharsetList NoCharset = []
--   toCharsetList (CharsetsProvided xs) = NEL.toList xs

defaultErrorRenderer :: (Monad m, HasRequestInfo rq) => ErrorRenderer rq rb s m
defaultErrorRenderer (s, e) = liftM Just (render s) where
  render (Status 404 _) = do
    putRespHeader hContentType "text/html"
    return $ BuilderResponseBody $ byteString "<html><head><title>404 Not Found</title></head><body><h1>Not Found</h1>The requested document was not found on this server.<p><hr><address>webcrank web server</address></body></html>"
  render (Status 501 _) = do
    putRespHeader hContentType "text/html"
    m <- getRqMethod
    return $ BuilderResponseBody $ mconcat [ byteString "<html><head><title>501 Not Implemented</title></head><body><h1>Not Implemented</h1>The server does not support the "
                                           , byteString m
                                           , byteString " method.<p><hr><address>webcrank web server</address></body></html>"
                                           ]
  render (Status 503 _) = do
    putRespHeader hContentType "text/html"
    return $ BuilderResponseBody $ byteString "<html><head><title>503 Service Unavailable</title></head><body><h1>Service Unavailable</h1>The server is currently unable to handle the request due to a temporary overloading or maintenance of the server.<p><hr><address>webcrank web server</address></body></html>"
  render (Status c msg) = do
    putRespHeader hContentType "text/html"
    return $ BuilderResponseBody $ mconcat
      [ byteString "<html><head><title>"
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
  , respEncoding  = Nothing
  , respHdrs      = []
  , respBody      = Nothing
  }

