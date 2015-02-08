{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Webcrank.Internal.DecisionCore where

import Blaze.ByteString.Builder
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Bool
import Data.ByteString (ByteString)
import qualified Data.ByteString as B hiding (drop, take)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as B
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import Network.HTTP.Date
import Network.HTTP.Media
import Network.HTTP.Types

import Webcrank.Internal.ETag
import Webcrank.Internal.Types
import Webcrank.Internal.ReqData
import Webcrank.Internal.ReqState
import Webcrank.Internal.ServerAPI

data FlowChart m a where
  Decision :: String -> m (FlowChart m a) -> FlowChart m a
  Done :: m a -> FlowChart m a

class (Applicative m, Monad m) => Monad' m

instance (Applicative m, Monad m) => Monad' m

decision :: String               -- label
         -> m (FlowChart m a)    -- next step
         -> FlowChart m a
decision = Decision

decision' :: Functor m
          => String        -- label
          -> m Bool        -- condition
          -> FlowChart m a -- false path
          -> FlowChart m a -- true path
          -> FlowChart m a
decision' lbl cond ff tf = decision lbl (bool ff tf <$> cond)

done :: m a -> FlowChart m a
done = Done

done' :: Monad' m => a -> FlowChart m a
done' = Done . return

runFlowChart :: Monad m => FlowChart m a -> m a
runFlowChart = \case
  Decision _ m -> m >>= runFlowChart
  Done m -> m

callr :: MonadReader a m => (a -> m b) -> m b
callr = (ask >>=)

callr' :: Monad m => (Resource s m -> ReqState' s m a) -> ReqState s m a
callr' = ReqState . lift . callr

callrm :: Monad m => (Resource s m -> MaybeT (ReqState' s m) a) -> MaybeT (ReqState s m) a
callrm = mapMaybeT (ReqState . lift) . callr

fromMaybeT :: Functor m => a -> MaybeT m a -> m a
fromMaybeT x = fmap (fromMaybe x) . runMaybeT

respond :: Monad' m => Status -> FlowChart (ReqState s m) Status
respond s =
  if statusCode s >= 400 && statusCode s < 600
    then done $ werror' s
    else done' s

errorResponse :: Monad m => Status -> LB.ByteString -> FlowChart (ReqState s m) a
errorResponse s = Done . werror s

hAcceptCharset, hAcceptEncoding, hAllow, hIfMatch, hIfNoneMatch, hIfUnmodifiedSince, hWWWAuthenticate :: HeaderName
hAcceptCharset = "Accept-Charset"
hAcceptEncoding = "Accept-Encoding"
hAllow = "Allow"
hIfMatch = "If-Match"
hIfNoneMatch = "If-None-Match"
hIfUnmodifiedSince = "If-Unmodified-Since"
hWWWAuthenticate = "WWW-Authenticate"

-- Service Available
b13 :: Monad' m => FlowChart (ReqState s m) Status
b13 = decision' "b13" (callr serviceAvailable) (respond serviceUnavailable503) b12

-- Known method?
b12 :: Monad' m => FlowChart (ReqState s m) Status
b12 = decision' "b12" knownMethod (respond notImplemented501) b11 where
  knownMethod = (`elem` knownMethods) <$> getRequestMethod
  -- TODO make it part of the config or part of the resource?
  knownMethods = [methodGet, methodHead, methodPost, methodPut, methodDelete, methodTrace, methodConnect, methodOptions]

-- URI too long?
b11 :: Monad' m => FlowChart (ReqState s m) Status
b11 = decision' "b11" (callr uriTooLong) b10 (respond requestURITooLong414)

-- Method allowed?
b10 :: Monad' m => FlowChart (ReqState s m) Status
b10 = decision "b10" $ do
  ms <- callr' allowedMethods
  m <- getRequestMethod
  if m `elem` ms
     then return b9
     else do
       putResponseHeader hAllow (B.intercalate ", " ms)
       return $ respond methodNotAllowed405

-- Malformed?
b9 :: Monad' m => FlowChart (ReqState s m) Status
b9 = decision' "b9" (callr malformedRequest) b8 (respond badRequest400)

-- Authorized?
b8 :: Monad' m => FlowChart (ReqState s m) Status
b8 = decision "b8" $ callr isAuthorized >>= \case
  Authorized -> return b7
  Unauthorized h -> do
    putResponseHeader hWWWAuthenticate h
    return $ respond unauthorized401

-- Forbidden?
b7 :: Monad' m => FlowChart (ReqState s m) Status
b7 = decision' "b7" (callr forbidden) b6 (respond forbidden403)

-- Okay Content-* Headers?
b6 :: Monad' m => FlowChart (ReqState s m) Status
b6 = decision' "b6" (callr validContentHeaders) (respond notImplemented501) b5

-- Known Content-Type?
b5 :: Monad' m => FlowChart (ReqState s m) Status
b5 = decision' "b5" (callr knownContentType) (respond unsupportedMediaType415) b4

-- Req Entity Too Large?
b4 :: Monad' m => FlowChart (ReqState s m) Status
b4 = decision' "b4" (callr validEntityLength) (respond requestEntityTooLarge413) b3

-- OPTIONS?
b3 :: Monad' m => FlowChart (ReqState s m) Status
b3 = decision "b3" $ getRequestMethod >>= \m ->
  if m == methodOptions
    then respond ok200 <$ (callr' options >>= putResponseHeaders)
    else return c3

-- Accept exists?
c3 :: Monad' m => FlowChart (ReqState s m) Status
c3 = decision "c3" $ getRequestHeader hAccept >>= maybe d4' (return . c4) where
  d4' = do
    ts <- callr' contentTypesProvided
    traverse_ (putResponseMediaType . fst) (listToMaybe ts)
    return d4

-- Acceptable media type available?
c4 :: Monad' m => ByteString -> FlowChart (ReqState s m) Status
c4 acc = decision "c4" $ maybe (return noAcc) d4' =<< match where
  d4' = (d4 <$) . putResponseMediaType
  match = flip matchAccept acc . (fst <$>) <$> callr' contentTypesProvided
  noAcc = errorResponse notAcceptable406 "No acceptable media type available"

-- Accept-Language exists?
d4 :: Monad' m => FlowChart (ReqState s m) Status
d4 = decision "d4" $ maybe e5 d5 <$> getRequestHeader hAcceptLanguage

-- Acceptable Language available?
-- TODO implement proper conneg
d5 :: Monad' m => ByteString -> FlowChart (ReqState s m) Status
d5 _ = decision "d5" $ return e5

-- Accept-Charset exists?
e5 :: Monad' m => FlowChart (ReqState s m) Status
e5 = decision "e5" $ getRequestHeader hAcceptCharset >>=
  maybe (f6 <$ setCharsetFrom "*") (return . e6)

-- Acceptable Charset available?
e6 :: Monad' m => ByteString -> FlowChart (ReqState s m) Status
e6 acc = decision "e6" $ f6 <$ setCharsetFrom acc

setCharsetFrom :: Monad' m
               => ByteString
               -> ReqState s m ()
setCharsetFrom acc = callr' charsetsProvided >>= match where
  match = \case
    NoCharset -> return ()
    CharsetsProvided cs -> match' (fst <$> NE.toList cs)
  match' = maybe noAcc matched . flip matchAccept acc
  matched = putResponseCharset . Just
  noAcc = werror notAcceptable406 "No acceptable charset available"

-- Accept-Encoding exists?
f6 :: Monad' m => FlowChart (ReqState s m) Status
f6 = decision "f6" $ maybe g7 f7 <$> getRequestHeader hAcceptEncoding

-- Acceptable encoding available?
--
-- Note: This is a departure from webmachine and the activity diagram.
-- Webcrank will NEVER give a "406 Not Acceptable" response if an encoding
-- cannot be found.
--
--   If an Accept-Encoding header field is present in a request and none of
--   the available representations for the response have a content-coding
--   that is listed as acceptable, the origin server SHOULD send a response
--   without any content-coding.
--
-- http://tools.ietf.org/html/draft-ietf-httpbis-p2-semantics-24#section-5.3.4
f7 :: Monad' m => ByteString -> FlowChart (ReqState s m) Status
f7 acc = decision "f7" $ g7 <$ setEncoding where
  setEncoding = callr' encodingsProvided >>=
    traverse_ putResponseEncoding . flip matchAccept acc . (fst <$>)

-- Resource exists?
g7 :: Monad' m => FlowChart (ReqState s m) Status
g7 = decision' "g7" (callr resourceExists) h7 g8

-- If-Match exists?
g8 :: Monad' m => FlowChart (ReqState s m) Status
g8 = decision "g8" $ maybe h10 g9 <$> getRequestHeader hIfMatch

-- If-Match: * exists
g9 :: Monad' m => ByteString -> FlowChart (ReqState s m) Status
g9 h = decision "g9" $ return $ bool (g11 h) h10 (h == "*")

-- ETag in If-Match
g11 :: Monad' m => ByteString -> FlowChart (ReqState s m) Status
g11 h = decision "g11" $ fromMaybeT (respond preconditionFailed412) $ do
  e <- callrm generateETag
  if any (strongComparison e) (parseETags h)
    then return h10
    else mzero

-- If-Match exists (no existing resource variant)?
h7 :: Monad' m => FlowChart (ReqState s m) Status
h7 = decision "h7" $ maybe i7 (const $ respond preconditionFailed412) <$> getRequestHeader hIfMatch

-- If-Unmodified-Since exists?
h10 :: Monad' m => FlowChart (ReqState s m) Status
h10 = decision "h10" $ maybe i12 h11 <$> getRequestHeader hIfUnmodifiedSince

-- If-Unmodified-Since is valid date?
h11 :: Monad' m => ByteString -> FlowChart (ReqState s m) Status
h11 = decision "h11" . return . maybe i12 h12 . parseHTTPDate

-- Last-Modified > If-Unmodified-Since?
h12 :: Monad' m => HTTPDate -> FlowChart (ReqState s m) Status
h12 d = decision "h12" $ fromMaybeT (respond preconditionFailed412) $ do
  lm <- callrm lastModified
  if lm > d
    then mzero
    else return i12

-- Moved permanently? (apply PUT to different URI)
i4 :: Monad' m => FlowChart (ReqState s m) Status
i4 = decision "i4" $ movedPermanentlyOr p3

movedPermanentlyOr
  :: (Monad' m, Monad' n)
  => FlowChart (ReqState s n) Status
  -> ReqState s m (FlowChart (ReqState s n) Status)
movedPermanentlyOr n = fromMaybeT n $ do
  uri <- callr movedPermanently
  lift $ putResponseLocation uri
  return $ respond movedPermanently301

-- PUT?
i7 :: Monad' m => FlowChart (ReqState s m) Status
i7 = decision "i7" $ bool k7 i4 . (== methodPut) <$> getRequestMethod

-- If-None-Match exists?
i12 :: Monad' m => FlowChart (ReqState s m) Status
i12 = decision "i12" $ maybe l13 i13 <$> getRequestHeader hIfNoneMatch

-- If-None-Match: * exists?
i13 :: Monad' m => ByteString -> FlowChart (ReqState s m) Status
i13 h = decision "i13" $ return $ bool (k13 h) j18 (h == "*")

-- GET or HEAD (resource exists)?
j18 :: Monad' m => FlowChart (ReqState s m) Status
j18 = decision "j18" $ respond . s <$> getRequestMethod where
  s = bool preconditionFailed412 notModified304 . (`elem` [methodGet, methodHead])

-- Moved permanently? (non-PUT edition)
k5 :: Monad' m => FlowChart (ReqState s m) Status
k5 = decision "k5" $ movedPermanentlyOr l5

-- Previously existed?
k7 :: Monad' m => FlowChart (ReqState s m) Status
k7 = decision "k7" $ bool l7 k5 <$> callr previouslyExisted

-- Etag in if-none-match?
k13 :: Monad' m => ByteString -> FlowChart (ReqState s m) Status
k13 h = decision "k13" $ fromMaybeT l13 $
  const j18 <$> mfilter (`elem` parseETags h) (callrm generateETag)

-- Moved temporarily?
l5 :: Monad' m => FlowChart (ReqState s m) Status
l5 = decision "l5" $ fromMaybeT m5 $ do
  uri <- callr movedTemporarily
  lift $ putResponseLocation uri
  return $ respond temporaryRedirect307

-- POST? (resource did not previously exist variant)
l7 :: Monad' m => FlowChart (ReqState s m) Status
l7 = decision' "l7" ((== methodPost) <$> getRequestMethod) (respond notFound404) m7

-- If-Modified-Since exists?
l13 :: Monad' m => FlowChart (ReqState s m) Status
l13 = decision "l13" $ maybe m16 l14 <$> getRequestHeader hIfModifiedSince

-- If-Modified-Since is a valid date?
l14 :: Monad' m => ByteString -> FlowChart (ReqState s m) Status
l14 = decision "l14" . return . maybe m16 l15 . parseHTTPDate

-- If-Modified-Since > Now?
l15 :: Monad' m => HTTPDate -> FlowChart (ReqState s m) Status
l15 d = decision' "l15" ((d >) <$> getRequestTime) (l17 d) m16

-- Last-Modified > If-Modified-Since?
l17 :: Monad' m => HTTPDate -> FlowChart (ReqState s m) Status
l17 ims = decision "l17" $ flip fmap (runMaybeT (callrm lastModified)) $ \case
  Just lm | lm > ims -> m16
  Just _ -> respond notModified304
  Nothing -> m16

-- POST? (resource previously existed variant)
m5 :: Monad' m => FlowChart (ReqState s m) Status
m5 = decision' "m5" ((== methodPost) <$> getRequestMethod) (respond gone410) n5

-- Server allows POST to missing resource?
m7 :: Monad' m => FlowChart (ReqState s m) Status
m7 = decision' "m7" (callr allowMissingPost) (respond notFound404) n11

-- DELETE?
m16 :: Monad' m => FlowChart (ReqState s m) Status
m16 = decision' "m16" ((== methodDelete) <$> getRequestMethod) n16 m20

-- DELETE and check for completion?
m20 :: Monad' m => FlowChart (ReqState s m) Status
m20 = decision "m20" $ callr deleteResource >>= \r ->
  if r
    then bool (respond accepted202) n11 <$> callr deleteCompleted
    else return $ respond internalServerError500

-- Server allows POST to missing resource? (resource did not exist previously)
n5 :: Monad' m => FlowChart (ReqState s m) Status
n5 = decision' "n5" (callr allowMissingPost) (respond gone410) n11

-- Redirect?
n11 :: Monad' m => FlowChart (ReqState s m) Status
n11 = decision "n11" $ callr' postAction >>= run where
  run = \case
    PostCreate p ->
      p11 <$ create p
    PostCreateRedir p ->
      respond seeOther303 <$ create p
    PostProcess process ->
      p11 <$ process
    PostProcessRedir process ->
      respond seeOther303 <$ (process >>= putResponseLocation)
  create newPath = do
    reqURI <- getRequestURI
    putDispatchPath newPath
    putResponseLocation $ appendPath reqURI newPath
    accept

appendPath :: ByteString -> [Text] -> ByteString
appendPath uri p = h <> p'' where
  (h, p') = splitURI uri
  p'' = p' <> dropSlash (toByteString (encodePathSegments p))
  dropSlash = B.drop (if B.last p' == 47 then 1 else 0)

splitURI :: ByteString -> (ByteString, ByteString)
splitURI = ensureNonEmpty . extract where
  extract path
    | "http://" `B.isPrefixOf` path = split 7 path
    | "https://" `B.isPrefixOf` path = split 8 path
    | otherwise = ("", path)
  ensureNonEmpty (b, "") = (b, "/")
  ensureNonEmpty p  = p
  split i path = case breakOnSlash $ B.drop i path of
    (a, p) -> (B.take i path <> a, p)
  breakOnSlash = B.breakByte 47

-- POST? (resource exists)
n16 :: Monad' m => FlowChart (ReqState s m) Status
n16 = decision' "n16" ((== methodPost) <$> getRequestMethod) o16 n11

-- Conflict? (resource exists)
o14 :: Monad' m => FlowChart (ReqState s m) Status
o14 = decision "o14" isConflict'

isConflict' :: Monad' m => ReqState s m (FlowChart (ReqState s m) Status)
isConflict' = callr' isConflict >>= \conflict ->
  if conflict
    then return $ respond conflict409
    else p11 <$ accept

-- PUT? (resource exists)
o16 :: Monad' m => FlowChart (ReqState s m) Status
o16 = decision' "o16" ((== methodPut) <$> getRequestMethod) o18 o14

-- Multiple representations?
o18 :: Monad' m => FlowChart (ReqState s m) Status
o18 = decision' "o18" (callr multipleChoices) (respond ok200) (respond multipleChoices300)

-- Response includes an entity?
o20 :: Monad' m => FlowChart (ReqState s m) Status
o20 = decision "o20" $
  maybe (respond noContent204) (const o18) <$> getResponseBody

-- Conflict? (resource doesn't exist)
p3 :: Monad' m => FlowChart (ReqState s m) Status
p3 = decision "p3" isConflict'

-- New resource? (new if there is a location header)
p11 :: Monad' m => FlowChart (ReqState s m) Status
p11 = decision "p11" $
  maybe o20 (const $ respond created201) <$> getResponseLocation

accept :: Monad' m => ReqState s m ()
accept = getContentType >>= accept' where
  getContentType = fromMaybe "application/octet-stream" <$> getRequestHeader hContentType
  accept' ct = callr' contentTypesAccepted >>= \fs ->
    fromMaybe (werror' unsupportedMediaType415) (mapContentMedia fs ct)

