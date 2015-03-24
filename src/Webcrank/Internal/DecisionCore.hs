{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Webcrank.Internal.DecisionCore where

import qualified Blaze.ByteString.Builder as BB
import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString as B hiding (drop, take)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as B
import qualified Data.CaseInsensitive as CI
import Data.Foldable (find, traverse_)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import Network.HTTP.Date
import Network.HTTP.Media
import Network.HTTP.Types

import Webcrank.Internal.ETag
import Webcrank.Internal.Halt
import Webcrank.Internal.Headers
import Webcrank.Internal.Types
import Webcrank.Internal.ReqData
import Webcrank.Internal.ResourceData

data FlowChart m a where
  Decision :: String -> m (FlowChart m a) -> FlowChart m a
  Done :: m a -> FlowChart m a

decision
  :: String               -- label
  -> m (FlowChart m a)    -- next step
  -> FlowChart m a
decision = Decision

decision'
  :: Functor m
  => String        -- label
  -> m Bool        -- condition
  -> FlowChart m a -- false path
  -> FlowChart m a -- true path
  -> FlowChart m a
decision' lbl cond ff tf = decision lbl (bool ff tf <$> cond)

done :: m a -> FlowChart m a
done = Done

done' :: (Applicative m, Monad m) => a -> FlowChart m a
done' = Done . return

runFlowChart :: Monad m => FlowChart m a -> m a
runFlowChart = \case
  Decision _ m -> m >>= runFlowChart
  Done m -> m

respond :: Monad m => Status -> FlowChart (HaltT m) Status
respond s =
  if statusCode s >= 400 && statusCode s < 600
    then done $ werror' s
    else done' s

errorResponse :: Monad m => Status -> LB.ByteString -> FlowChart (HaltT m) a
errorResponse s = Done . werror s

-- Service Available
b13
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
b13 = decision' "b13" (callr serviceAvailable) (respond serviceUnavailable503) b12

-- Known method?
b12
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
b12 = decision' "b12" knownMethod (respond notImplemented501) b11 where
  knownMethod = (`elem` knownMethods) <$> getRequestMethod
  -- TODO make it part of the config or part of the resource?
  knownMethods = [methodGet, methodHead, methodPost, methodPut, methodDelete, methodTrace, methodConnect, methodOptions]

-- URI too long?
b11
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
b11 = decision' "b11" (callr uriTooLong) b10 (respond requestURITooLong414)

-- Method allowed?
b10
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
b10 = decision "b10" $ do
  ms <- callr' allowedMethods
  m <- getRequestMethod
  if m `elem` ms
     then return b9
     else do
       putResponseHeader hAllow (B.intercalate ", " ms)
       return $ respond methodNotAllowed405

-- Malformed?
b9
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
b9 = decision' "b9" (callr malformedRequest) b8 (respond badRequest400)

-- Authorized?
b8
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
b8 = decision "b8" $ callr isAuthorized >>= \case
  Authorized -> return b7
  Unauthorized h -> do
    putResponseHeader hWWWAuthenticate h
    return $ respond unauthorized401

-- Forbidden?
b7
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
b7 = decision' "b7" (callr forbidden) b6 (respond forbidden403)

-- Okay Content-* Headers?
b6
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
b6 = decision' "b6" (callr validContentHeaders) (respond notImplemented501) b5

-- Known Content-Type?
b5
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
b5 = decision' "b5" (callr knownContentType) (respond unsupportedMediaType415) b4

-- Req Entity Too Large?
b4
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
b4 = decision' "b4" (callr validEntityLength) (respond requestEntityTooLarge413) b3

-- OPTIONS?
b3
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
b3 = decision "b3" $ getRequestMethod >>= \m ->
  if m == methodOptions
    then respond ok200 <$ (callr' options >>= putResponseHeaders)
    else return c3

-- Accept exists?
c3
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
c3 = decision "c3" $ getRequestHeader hAccept >>= maybe d4' (return . c4) where
  d4' = do
    ts <- callr' contentTypesProvided
    traverse_ (assign reqDataRespMediaType . fst) (listToMaybe ts)
    return d4

-- Acceptable media type available?
c4
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => ByteString
  -> FlowChart (HaltT m) Status
c4 acc = decision "c4" $ maybe (return noAcc) d4' =<< match where
  d4' = (d4 <$) . assign reqDataRespMediaType
  match = flip matchAccept acc . fmap fst <$> callr' contentTypesProvided
  noAcc = errorResponse notAcceptable406 "No acceptable media type available"

-- Accept-Language exists?
d4
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
d4 = decision "d4" $ maybe e5 d5 <$> getRequestHeader hAcceptLanguage

-- Acceptable Language available?
-- TODO implement proper conneg
d5
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => ByteString
  -> FlowChart (HaltT m) Status
d5 _ = decision "d5" $ return e5

-- Accept-Charset exists?
e5
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
e5 = decision "e5" $ getRequestHeader hAcceptCharset >>=
  maybe (f6 <$ setCharsetFrom "*") (return . e6)

-- Acceptable Charset available?
e6
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => ByteString
  -> FlowChart (HaltT m) Status
e6 acc = decision "e6" $ f6 <$ setCharsetFrom acc

setCharsetFrom
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => ByteString
  -> HaltT m ()
setCharsetFrom acc = callr' charsetsProvided >>= match where
  match = \case
    NoCharset -> return ()
    CharsetsProvided cs -> match' (fst <$> NE.toList cs)
  match' = maybe noAcc matched . flip matchAccept acc
  matched = assign reqDataRespCharset . Just
  noAcc = werror notAcceptable406 "No acceptable charset available"

-- Accept-Encoding exists?
-- also set Content-Type header now that charset is chosen
f6
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
f6 = decision "f6" $ do
  putResponseHeader hContentType =<< do
    mt <- use reqDataRespMediaType
    cs <- use reqDataRespCharset
    return $ renderHeader $ maybe mt ((mt /:) . ("charset",) . CI.original) cs

  acc <- getRequestHeader hAcceptEncoding
  maybe (g7 <$ chooseEncoding "identity;q=1.0,*,q=0.5") (return . f7) acc

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
f7
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => ByteString
  -> FlowChart (HaltT m) Status
f7 acc = decision "f7" $ g7 <$ chooseEncoding acc

chooseEncoding
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => ByteString
  -> HaltT m ()
chooseEncoding acc = callr' encodingsProvided >>= choose where
  choose = traverse_ putEnc . match . (fst <$>)
  match es = matchAccept es acc >>= \case
    "identity" -> Nothing
    e -> Just e
  putEnc e = do
    putResponseHeader hContentEncoding (CI.original e)
    reqDataRespEncoding .= Just e

-- Resource exists?
-- also sets variances now that all conneg is done
g7
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
g7 = decision "g7" $ do
   getVariances >>= \case
     [] -> return ()
     vs -> putResponseHeader hVary $ renderHeader vs

   bool h7 g8 <$> callr resourceExists

getVariances
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => HaltT m [HeaderName]
getVariances = do
  acc <- bool [] [hAccept] . (> 1) . List.length <$> callr' contentTypesProvided
  accEnc <- bool [] [hAcceptEncoding] . (> 1) . List.length <$> callr' encodingsProvided
  accCh <- flip fmap (callr' charsetsProvided) $ \case
    NoCharset -> []
    CharsetsProvided cs -> [hAcceptCharset | NE.length cs > 1]
  vs <- callr' variances
  return $ mconcat [acc, accEnc, accCh, vs]

-- If-Match exists?
g8
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
g8 = decision "g8" $ maybe h10 g9 <$> getRequestHeader hIfMatch

-- If-Match: * exists
g9
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => ByteString
  -> FlowChart (HaltT m) Status
g9 h = decision "g9" $ return $ bool (g11 h) h10 (h == "*")

-- ETag in If-Match
g11
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => ByteString
  -> FlowChart (HaltT m) Status
g11 h = decision "g11" $ check <$> callr' (runMaybeT . generateETag) where
  check = maybe (respond preconditionFailed412) (const h10) . mfilter test
  test e = any (strongComparison e) (parseETags h)

-- If-Match exists (no existing resource variant)?
h7
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
h7 = decision "h7" $ maybe i7 (const $ respond preconditionFailed412) <$> getRequestHeader hIfMatch

-- If-Unmodified-Since exists?
h10
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
h10 = decision "h10" $ maybe i12 h11 <$> getRequestHeader hIfUnmodifiedSince

-- If-Unmodified-Since is valid date?
h11
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => ByteString
  -> FlowChart (HaltT m) Status
h11 = decision "h11" . return . maybe i12 h12 . parseHTTPDate

-- Last-Modified > If-Unmodified-Since?
h12
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => HTTPDate
  -> FlowChart (HaltT m) Status
h12 ius = decision "h12" $ check <$> callr' (runMaybeT . lastModified) where
  check = maybe (respond preconditionFailed412) (const i12) . mfilter (<= ius)

-- Moved permanently? (apply PUT to different URI)
i4
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
i4 = decision "i4" $ movedPermanentlyOr p3

movedPermanentlyOr
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
  -> HaltT m (FlowChart (HaltT m) Status)
movedPermanentlyOr n = check =<< callr (runMaybeT . movedPermanently) where
  check = maybe (return n) moved
  moved uri = respond movedPermanently301 <$ putResponseLocation uri

-- PUT?
i7
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
i7 = decision "i7" $ bool k7 i4 . (== methodPut) <$> getRequestMethod

-- If-None-Match exists?
i12
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
i12 = decision "i12" $ maybe l13 i13 <$> getRequestHeader hIfNoneMatch

-- If-None-Match: * exists?
i13
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => ByteString
  -> FlowChart (HaltT m) Status
i13 h = decision "i13" $ return $ bool (k13 h) j18 (h == "*")

-- GET or HEAD (resource exists)?
j18
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
j18 = decision "j18" $ respond . s <$> getRequestMethod where
  s = bool preconditionFailed412 notModified304 . (`elem` [methodGet, methodHead])

-- Moved permanently? (non-PUT edition)
k5
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
k5 = decision "k5" $ movedPermanentlyOr l5

-- Previously existed?
k7
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
k7 = decision "k7" $ bool l7 k5 <$> callr previouslyExisted

-- Etag in if-none-match?
k13
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => ByteString
  -> FlowChart (HaltT m) Status
k13 h = decision "k13" $ check <$> callr' (runMaybeT . generateETag) where
  check = maybe l13 (const j18) . mfilter (`elem` inm)
  inm = parseETags h

-- Moved temporarily?
l5
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
l5 = decision "l5" $ callr (runMaybeT . movedTemporarily) >>= check where
  check = maybe (return m5) redirect
  redirect uri = respond temporaryRedirect307 <$ putResponseLocation uri

-- POST? (resource did not previously exist variant)
l7
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
l7 = decision' "l7" ((== methodPost) <$> getRequestMethod) (respond notFound404) m7

-- If-Modified-Since exists?
l13
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
l13 = decision "l13" $ maybe m16 l14 <$> getRequestHeader hIfModifiedSince

-- If-Modified-Since is a valid date?
l14
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => ByteString
  -> FlowChart (HaltT m) Status
l14 = decision "l14" . return . maybe m16 l15 . parseHTTPDate

-- If-Modified-Since > Now?
l15
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => HTTPDate
  -> FlowChart (HaltT m) Status
l15 ims = decision' "l15" ((ims >) <$> getRequestTime) (l17 ims) m16

-- Last-Modified > If-Modified-Since?
l17
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => HTTPDate
  -> FlowChart (HaltT m) Status
l17 ims = decision "l17" $ check <$> callr' (runMaybeT . lastModified) where
  check = maybe m16 (const $ respond notModified304) . mfilter (<= ims)

-- POST? (resource previously existed variant)
m5
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
m5 = decision' "m5" ((== methodPost) <$> getRequestMethod) (respond gone410) n5

-- Server allows POST to missing resource?
m7
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
m7 = decision' "m7" (callr allowMissingPost) (respond notFound404) n11

-- DELETE?
m16
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
m16 = decision' "m16" ((== methodDelete) <$> getRequestMethod) n16 m20

-- DELETE and check for completion?
m20
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
m20 = decision "m20" $ callr deleteResource >>= \r ->
  if r
    then bool (respond accepted202) n11 <$> callr deleteCompleted
    else return $ respond internalServerError500

-- Server allows POST to missing resource? (resource did not exist previously)
n5
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
n5 = decision' "n5" (callr allowMissingPost) (respond gone410) n11

-- Redirect?
n11
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
n11 = decision "n11" $ callr' postAction >>= run where
  run = \case
    PostCreate p ->
      p11 <$ create p
    PostCreateRedir p ->
      respond seeOther303 <$ create p
    PostProcess process ->
      p11 <$ (process >> encodeBodyIfSet)
    PostProcessRedir process ->
      respond seeOther303 <$ (process >>= putResponseLocation >> encodeBodyIfSet)

  create newPath = do
    reqURI <- getRequestURI
    reqDataDispPath .= newPath
    putResponseLocation $ appendPath reqURI newPath
    accept

appendPath :: ByteString -> [Text] -> ByteString
appendPath uri p = h <> p'' where
  (h, p') = splitURI uri
  p'' = p' <> dropSlash (BB.toByteString (encodePathSegments p))
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
n16
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
n16 = decision' "n16" ((== methodPost) <$> getRequestMethod) o16 n11

-- Conflict? (resource exists)
o14
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
o14 = decision "o14" isConflict'

isConflict'
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => HaltT m (FlowChart (HaltT m) Status)
isConflict' = callr' isConflict >>= \conflict ->
  if conflict
    then return $ respond conflict409
    else p11 <$ accept

-- PUT? (resource exists)
o16
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
o16 = decision' "o16" ((== methodPut) <$> getRequestMethod) o18 o14

-- Multiple representations?
-- also generate body for GET and HEAD
o18
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
o18 = decision "o18" $ genBody >> next where
  genBody = do
    m <- getRequestMethod
    putHeaders m
    putBody m

  putHeaders m = when (m == methodGet || m == methodHead) $ do
    let header h rm = traverse_ (putResponseHeader h . renderHeader) =<< callr' (runMaybeT . rm)
    header hETag generateETag
    header hLastModified lastModified
    header hExpires expires

  putBody m = when (m == methodGet) $ use reqDataRespMediaType >>= \mt ->
    callr' contentTypesProvided >>= \cts ->
      case find ((mt ==) . fst) cts of
        Nothing -> return ()
        Just (_, f) -> f >>= encodeBody >>= assign reqDataRespBody . Just

  next = bool (respond ok200) (respond multipleChoices300) <$> callr multipleChoices

-- Response includes an entity?
o20
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
o20 = decision "o20" $
  maybe (respond noContent204) (const o18) <$> use reqDataRespBody

-- Conflict? (resource doesn't exist)
p3
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
p3 = decision "p3" isConflict'

-- New resource? (new if there is a location header)
p11
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => FlowChart (HaltT m) Status
p11 = decision "p11" $
  maybe o20 (const $ respond created201) <$> getResponseLocation

accept
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => HaltT m ()
accept = getRequestContentType >>= accept' >> encodeBodyIfSet where
  getRequestContentType =
    fromMaybe "application/octet-stream" <$> getRequestHeader hContentType
  accept' ct = callr' contentTypesAccepted >>= \fs ->
    fromMaybe (werror' unsupportedMediaType415) (mapContentMedia fs ct)

bool :: a -> a -> Bool -> a
bool x y p = if p then y else x

(<%%=):: MonadState s m => Lens' s a -> (a -> m a) -> m ()
l <%%= f = use l >>= f >>= assign l

encodeBodyIfSet
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => HaltT m ()
encodeBodyIfSet = reqDataRespBody <%%= traverse encodeBody

encodeBody
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => Body
  -> HaltT m Body
encodeBody = lift . encodeBody'

encodeBody'
  :: (Applicative m, MonadReader r m, HasResourceData r m, MonadState s m, HasReqData s)
  => Body
  -> m Body
encodeBody' b = do
  cs <- use reqDataRespCharset >>= \case
    Nothing -> return id
    Just cs -> callr'' charsetsProvided <&> \case
      NoCharset -> id
      CharsetsProvided cps ->
        case find ((cs ==) . fst) cps of
          Nothing -> id
          Just (_, x) -> x
  enc <- use reqDataRespEncoding >>= \case
    Nothing -> return id
    Just e -> callr'' encodingsProvided <&> \es ->
      case find ((e ==) . fst) es of
        Nothing -> id
        Just (_, x) -> x
  return $ enc $ cs b

