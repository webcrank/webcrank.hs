{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Webcrank.Internal.Types where

import Control.Applicative
import Control.Lens
import Control.Monad.Catch
import Control.Monad.RWS
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as B
import Data.CaseInsensitive (CI)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Network.HTTP.Date
import Network.HTTP.Media
import Network.HTTP.Types

import Webcrank.Internal.Headers

-- | A dictionary of functions that Webcrank needs in order to make decisions.
data ServerAPI m = ServerAPI
  { srvGetRequestMethod :: m Method
    -- ^ Get the request method of the current request.

  , srvGetRequestURI :: m ByteString
    -- ^ The full URI of the request.

  , srvGetRequestHeader :: HeaderName -> m (Maybe ByteString)
    -- ^ Get the request header of the current request.

  , srvGetRequestTime :: m HTTPDate
    -- ^ Get the time the request was received.
  }

type HeadersMap = HashMap HeaderName [ByteString]

-- | Content coding type, e.g. gzip, decompress. See @'encodingsProvided'@.
type Encoding = CI ByteString

-- | Character set type, e.g. utf-8. See @'charsetsProvided'@.
type Charset = CI ByteString

-- | Response body type.
type Body = LB.ByteString

data ReqData m = ReqData
  { _reqDataServerAPI :: ServerAPI m
  , _reqDataRespMediaType :: MediaType
  , _reqDataRespCharset :: Maybe Charset
  , _reqDataRespEncoding :: Maybe Encoding
  , _reqDataDispPath :: [Text]
  , _reqDataRespHeaders :: HeadersMap
  , _reqDataRespBody :: Maybe Body
  }

makeFields ''ReqData

-- | A simpler version of the @'WebcrankT'@ transformer which does
-- not allow for early termination.
newtype WebcrankT' m a = WebcrankT' { unWebcrankT' :: RWST (Resource m) () (ReqData m) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (Resource m)
    , MonadState (ReqData m)
    , MonadThrow
    , MonadCatch
    )

instance MonadTrans WebcrankT' where
  lift = WebcrankT' . lift

data Halt = Halt Status | Error Status (Maybe LB.ByteString)
  deriving (Eq, Show)

-- | Monad transformer that the Webcrank decision process runs in.
-- Tracks the decisions made so far, handles errors, and allows early
-- termination. Any resource function can stop processing early with
-- @'halt'@ or @'werror'@.
newtype WebcrankT m a = WebcrankT { unWebcrankT :: EitherT Halt (WebcrankT' m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (Resource m)
    , MonadState (ReqData m)
    , MonadThrow
    , MonadCatch
    )

instance MonadTrans WebcrankT where
  lift = WebcrankT . lift . lift

-- | Indicates whether client is authorized to perform the requested
-- operation on the resource. See @'isAuthorized'@.
data Authorized
  = Authorized
    -- ^ Tells Webcrank that the client is authorized to perform the
    -- requested operation on the resource.
  | Unauthorized ByteString
    -- ^ Tells Webcrank that the client is not authorized to perform
    -- the operation on the resource. The value is sent in the
    -- @WWW-Authenticate@ header of the response,
    -- e.g. @Basic realm="Webcrank"@.

-- | Indicates whether the resource supports multiple character sets
-- or not.  See @'charsetsProvided'@
data CharsetsProvided
  = NoCharset
    -- ^ Indicates that the resource doesn't support any additional
    -- character sets, all responses from the resource will have the
    -- same character set, regardless of what the client requests.
  | CharsetsProvided (NonEmpty (Charset, Body -> Body))
    -- ^ The character sets the resource supports along with functions
    -- for converting the response body.

-- | Weak or strong entity tags as used in HTTP ETag and If-*-Match headers.
data ETag = StrongETag ByteString | WeakETag ByteString deriving Eq

instance Show ETag where
  show e = B.toString $ case e of
    StrongETag v -> "\"" <> v <> "\""
    WeakETag v -> "W/\"" <> v <> "\""

instance RenderHeader ETag where
  renderHeader = \case
    StrongETag v -> quotedString v
    WeakETag v -> "W/" <> quotedString v

-- | How @POST@ requests should be treated. See @'postAction'@.
data PostAction m
  = PostCreate [Text]
    -- ^ Treat @POST@s as creating new resources and respond
    -- with @201 Created@, with the given path in the Location header.
  | PostCreateRedir [Text]
    -- ^ Treat @POST@s as creating new resources and respond with
    -- @301 See Other@, redirecting the client to the new resource.
  | PostProcess (WebcrankT m ())
    -- ^ Treat @POST@s as a process which is executed without redirect.
  | PostProcessRedir (WebcrankT m ByteString)
    -- ^ Treat @POST@s as a process and redirect the client to a
    -- different (possibly new) resource.

-- | A @Resource@ is a dictionary of functions which are used in the Webcrank
-- decision process to determine how requests should be handled.
--
-- Each function has a type of either @'WebcrankT' m a@ or @'WebcrankT'' m a@.
-- These monad transformers track the state of the request as it is processed and
-- a response is generated. The difference between @WebcrankT@ and @WebcrankT'@
-- is that the former allows for early termination using @'halt'@ or
-- @'werror'@.
--
-- The specified defaults are used by @'resource'@ to create a simple resource.
-- It won't do anything useful, but it is a starting point. Defining a
-- simple resource that responds to @GET@ requests would look like
--
-- @
-- myResource = resource { contentTypesProvided = return $ [("text/html", return "Hello world!")] }
-- @
--
-- @'responseWithBody'@ and @'responseWithHtml'@ are
-- convience functions for creating such simple resources.
data Resource m = Resource
  { serviceAvailable :: WebcrankT m Bool
    -- ^ @False@ will result in @503 Service Unavailable@. Defaults to @True@.

  , uriTooLong :: WebcrankT m Bool
    -- ^ @True@ will result in @414 Request Too Long@. Defaults to @False@.

  , allowedMethods :: WebcrankT' m [Method]
    -- ^ If a @Method@ not in this list is requested, then a @405 Method Not
    -- Allowed@ will be sent. Defaults to @GET@ and @HEAD@.

  , malformedRequest :: WebcrankT m Bool
    -- ^ @True@ will result in @400 Bad Request@. Defaults to @False@.

  , isAuthorized :: WebcrankT m Authorized
    -- ^ If @Authorized@, the response will be @401 Unauthorized@.
    -- @Unauthorized@ will be used as the challenge in the @WWW-Authenticate@
    -- header, e.g. @Basic realm="Webcrank"@.
    -- Defaults to @Authorized@.

  , forbidden :: WebcrankT m Bool
    -- ^ @True@ will result in @403 Forbidden@. Defaults to @False@.

  , validContentHeaders :: WebcrankT m Bool
    -- ^ @False@ will result in @501 Not Implemented@. Defaults to @True@.

  , knownContentType :: WebcrankT m Bool
    -- ^ @False@ will result in @415 Unsupported Media Type@. Defaults to
    -- @True@.

  , validEntityLength :: WebcrankT m Bool
    -- ^ @False@ will result in @413 Request Entity Too Large@. Defaults to
    -- @True@.

  , options :: WebcrankT' m ResponseHeaders
    -- ^ If the OPTIONS method is supported and is used, the headers that
    -- should appear in the response.

  , contentTypesProvided :: WebcrankT' m [(MediaType, WebcrankT m Body)]
    -- ^ Content negotiation is driven by this function. For example, if a
    -- client request includes an @Accept@ header with a value that does not
    -- appear as a @MediaType@ in any of the tuples, then a @406 Not
    -- Acceptable@ will be sent. If there is a matching @MediaType@, that
    -- function is used to create the entity when a response should include one.

  , charsetsProvided :: WebcrankT' m CharsetsProvided
    -- ^ Used on GET requests to ensure that the entity is in @Charset@.

  , encodingsProvided :: WebcrankT' m [(Encoding, Body -> Body)]
    -- ^ Used on GET requests to ensure that the body is encoded.
    -- One useful setting is to have the function check on method, and on GET
    -- requests return @[("identity", id), ("gzip", compress)]@ as this is all
    -- that is needed to support gzip content encoding.

  , resourceExists :: WebcrankT m Bool
    -- ^ @False@ will result in @404 Not Found@. Defaults to @True@.

  , generateETag :: MaybeT (WebcrankT' m) ETag
    -- ^ If this returns an @ETag@, it will be used for the ETag header and for
    -- comparison in conditional requests.

  , lastModified :: MaybeT (WebcrankT' m) HTTPDate
    -- ^ If this returns a @HTTPDate@, it will be used for the Last-Modified header
    -- and for comparison in conditional requests.

  , expires :: MaybeT (WebcrankT' m) HTTPDate
    -- ^ If this returns a @HTTPDate@, it will be used for the Expires header.

  , movedPermanently :: MaybeT (WebcrankT m) ByteString
    -- ^ If this returns a URI, the client will receive a 301 Moved Permanently
    -- with the URI in the Location header.

  , movedTemporarily :: MaybeT (WebcrankT m) ByteString
    -- ^ If this returns a URI, the client will receive a 307 Temporary Redirect
    -- with URI in the Location header.

  , previouslyExisted :: WebcrankT m Bool
    -- ^ If this returns @True@, the @movedPermanently@ and @movedTemporarily@
    -- callbacks will be invoked to determine whether the response should be
    -- 301 Moved Permanently, 307 Temporary Redirect, or 410 Gone.

  , allowMissingPost :: WebcrankT m Bool
    -- ^ If the resource accepts POST requests to nonexistent resources, then
    -- this should return @True@. Defaults to @False@.

  , deleteResource :: WebcrankT m Bool
    -- ^ This is called when a DELETE request should be enacted, and should return
    -- @True@ if the deletion succeeded or has been accepted.

  , deleteCompleted :: WebcrankT m Bool
    -- ^ This is only called after a successful @deleteResource@ call, and should
    -- return @False@ if the deletion was accepted but cannot yet be guaranteed to
    -- have finished.

  , postAction :: WebcrankT' m (PostAction m)
    -- ^ If POST requests should be treated as a request to put content into a
    -- (potentially new) resource as opposed to being a generic submission for
    -- processing, then this function should return @PostCreate path@. If it
    -- does return @PostCreate path@, then the rest of the request will be
    -- treated much like a PUT to the path entry. Otherwise, if it returns
    -- @PostProcess a@, then the action @a@ will be run.

  , contentTypesAccepted :: WebcrankT' m [(MediaType, WebcrankT m ())]

  , variances :: WebcrankT' m [HeaderName]
    -- ^ This function should return a list of strings with header names that
    -- should be included in a given response's Vary header. The standard
    -- conneg headers (Accept, Accept-Encoding, Accept-Charset,
    -- Accept-Language) do not need to be specified here as Webcrank will add
    -- the correct elements of those automatically depending on resource
    -- behavior.

  , multipleChoices :: WebcrankT m Bool
    -- ^ If this returns @True@, then it is assumed that multiple
    -- representations of the response are possible and a single one cannot
    -- be automatically chosen, so a @300 Multiple Choices@ will be sent
    -- instead of a @200 OK@.

  , isConflict :: WebcrankT' m Bool
    -- ^ If this returns @True@, the client will receive a 409 Conflict.

  , finishRequest :: WebcrankT' m ()
    -- ^ Called just before the final response is constructed and sent.
  }

