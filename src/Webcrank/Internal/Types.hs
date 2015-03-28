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

-- | Weak or strong entity tags as used in HTTP ETag and @If-*-Match@ headers.
data ETag = StrongETag ByteString | WeakETag ByteString deriving Eq

instance Show ETag where
  show e = B.toString $ case e of
    StrongETag v -> "\"" <> v <> "\""
    WeakETag v -> "W/\"" <> v <> "\""

instance RenderHeader ETag where
  renderHeader = \case
    StrongETag v -> quotedString v
    WeakETag v -> "W/" <> quotedString v

data Halt = Halt Status | Error Status LB.ByteString
  deriving (Eq, Show)

-- | Monad transformer for @'Resource'@ functions which can halt the request
-- processing early with an error or some other response. Values are created with
-- the smart constructors @'werror'@ and @'halt'@.
newtype HaltT m a = HaltT { unHaltT :: EitherT Halt m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadTrans
    , MonadReader r
    , MonadState s
    , MonadWriter w
    , MonadThrow
    , MonadCatch
    )

-- | How @POST@ requests should be treated. See @'postAction'@.
data PostAction m
  = PostCreate [Text]
    -- ^ Treat @POST@s as creating new resources and respond
    -- with @201 Created@, with the given path in the Location header.
  | PostCreateRedir [Text]
    -- ^ Treat @POST@s as creating new resources and respond with
    -- @301 See Other@, redirecting the client to the new resource.
  | PostProcess (HaltT m ())
    -- ^ Treat @POST@s as a process which is executed without redirect.
  | PostProcessRedir (HaltT m ByteString)
    -- ^ Treat @POST@s as a process and redirect the client to a
    -- different (possibly new) resource.

data LogData = LogData

instance Monoid LogData where
  mempty = LogData
  mappend _ _ = LogData

-- | A @Resource@ is a dictionary of functions which are used in the Webcrank
-- decision process to determine how requests should be handled.
--
-- Each function has a type of either @m a@ or @'HaltT' m a@.
-- A resource function which yields a @HaltT m a@ value allows the function
-- to terminate the request processing early using @'halt'@ or
-- @'werror'@.
--
-- The defaults documented are used by the @'resource'@ smart constructor.
-- A resource that responds to @GET@ requests with an HTML response would be
-- written as
--
-- @
-- myResource = resource { contentTypesProvided = return $ [("text/html", return "Hello world!")] }
-- @
--
-- @'responseWithBody'@ and @'responseWithHtml'@ are additional
-- smart constructors useful creating resources.
data Resource m = Resource
  { serviceAvailable :: HaltT m Bool
    -- ^ @False@ will result in @503 Service Unavailable@. Defaults to @True@.

  , uriTooLong :: HaltT m Bool
    -- ^ @True@ will result in @414 Request Too Long@. Defaults to @False@.

  , allowedMethods :: m [Method]
    -- ^ If a @Method@ not in this list is requested, then a @405 Method Not
    -- Allowed@ will be sent. Defaults to @["GET", "HEAD"]@.

  , malformedRequest :: HaltT m Bool
    -- ^ @True@ will result in @400 Bad Request@. Defaults to @False@.

  , isAuthorized :: HaltT m Authorized
    -- ^ If @Authorized@, the response will be @401 Unauthorized@.
    -- @Unauthorized@ will be used as the challenge in the @WWW-Authenticate@
    -- header, e.g. @Basic realm="Webcrank"@.
    -- Defaults to @Authorized@.

  , forbidden :: HaltT m Bool
    -- ^ @True@ will result in @403 Forbidden@. Defaults to @False@.

  , validContentHeaders :: HaltT m Bool
    -- ^ @False@ will result in @501 Not Implemented@. Defaults to @True@.

  , knownContentType :: HaltT m Bool
    -- ^ @False@ will result in @415 Unsupported Media Type@. Defaults to
    -- @True@.

  , validEntityLength :: HaltT m Bool
    -- ^ @False@ will result in @413 Request Entity Too Large@. Defaults to
    -- @True@.

  , options :: m ResponseHeaders
    -- ^ If the OPTIONS method is supported and is used, the headers that
    -- should appear in the response. Defaults to @[]@.

  , contentTypesProvided :: m [(MediaType, HaltT m Body)]
    -- ^ Content negotiation is driven by this function. For example, if a
    -- client request includes an @Accept@ header with a value that does not
    -- appear as a @MediaType@ in any of the tuples, then a @406 Not
    -- Acceptable@ will be sent. If there is a matching @MediaType@, that
    -- function is used to create the entity when a response should include one.
    -- Defaults to @[]@.

  , charsetsProvided :: m CharsetsProvided
    -- ^ Used on GET requests to ensure that the entity is in @Charset@.
    -- Defaults to @NoCharset@.

  , encodingsProvided :: m [(Encoding, Body -> Body)]
    -- ^ Used on GET requests to ensure that the body is encoded.
    -- One useful setting is to have the function check on method, and on GET
    -- requests return @[("identity", id), ("gzip", compress)]@ as this is all
    -- that is needed to support gzip content encoding. Defaults to
    -- @[]@.

  , resourceExists :: HaltT m Bool
    -- ^ @False@ will result in @404 Not Found@. Defaults to @True@.

  , generateETag :: MaybeT m ETag
    -- ^ If this returns an @ETag@, it will be used for the ETag header and for
    -- comparison in conditional requests. Defaults to @mzero@.

  , lastModified :: MaybeT m HTTPDate
    -- ^ If this returns a @HTTPDate@, it will be used for the Last-Modified header
    -- and for comparison in conditional requests. Defaults to @mzero@.

  , expires :: MaybeT m HTTPDate
    -- ^ If this returns a @HTTPDate@, it will be used for the Expires header.
    -- Defaults to @mzero@.

  , movedPermanently :: MaybeT (HaltT m) ByteString
    -- ^ If this returns a URI, the client will receive a 301 Moved Permanently
    -- with the URI in the Location header. Defaults to @mzero@.

  , movedTemporarily :: MaybeT (HaltT m) ByteString
    -- ^ If this returns a URI, the client will receive a 307 Temporary Redirect
    -- with URI in the Location header. Defaults to @mzero@.

  , previouslyExisted :: HaltT m Bool
    -- ^ If this returns @True@, the @movedPermanently@ and @movedTemporarily@
    -- callbacks will be invoked to determine whether the response should be
    -- 301 Moved Permanently, 307 Temporary Redirect, or 410 Gone. Defaults
    -- to @False@.

  , allowMissingPost :: HaltT m Bool
    -- ^ If the resource accepts POST requests to nonexistent resources, then
    -- this should return @True@. Defaults to @False@.

  , deleteResource :: HaltT m Bool
    -- ^ This is called when a DELETE request should be enacted, and should return
    -- @True@ if the deletion succeeded or has been accepted. Defaults to
    -- @True@.

  , deleteCompleted :: HaltT m Bool
    -- ^ This is only called after a successful @deleteResource@ call, and should
    -- return @False@ if the deletion was accepted but cannot yet be guaranteed to
    -- have finished. Defaults to @True@.

  , postAction :: m (PostAction m)
    -- ^ If POST requests should be treated as a request to put content into a
    -- (potentially new) resource as opposed to being a generic submission for
    -- processing, then this function should return @PostCreate path@. If it
    -- does return @PostCreate path@, then the rest of the request will be
    -- treated much like a PUT to the path entry. Otherwise, if it returns
    -- @PostProcess a@, then the action @a@ will be run. Defaults to
    -- @PostProcess $ return ()@.

  , contentTypesAccepted :: m [(MediaType, HaltT m ())]
    -- ^ This is used similarly to @contentTypesProvided@, except that it is
    -- for incoming resource representations -- for example, @PUT@ requests.
    -- Handler functions usually want to use server specific functions to
    -- access the incoming request body. Defaults to @[]@.

  , variances :: m [HeaderName]
    -- ^ This function should return a list of strings with header names that
    -- should be included in a given response's Vary header. The standard
    -- conneg headers (Accept, Accept-Encoding, Accept-Charset,
    -- Accept-Language) do not need to be specified here as Webcrank will add
    -- the correct elements of those automatically depending on resource
    -- behavior. Defaults to @[]@.

  , multipleChoices :: HaltT m Bool
    -- ^ If this returns @True@, then it is assumed that multiple
    -- representations of the response are possible and a single one cannot
    -- be automatically chosen, so a @300 Multiple Choices@ will be sent
    -- instead of a @200 OK@. Defaults to @False@.

  , isConflict :: m Bool
    -- ^ If this returns @True@, the client will receive a 409 Conflict.
    -- Defaults to @False@.

  , finishRequest :: m ()
    -- ^ Called just before the final response is constructed and sent.
  }

-- | A wrapper for the @'ServerAPI'@ and @'Resource'@ that should be used
-- to process requests to a path.
data ResourceData m = ResourceData
  { _resourceDataServerAPI :: ServerAPI m
  , _resourceDataResource :: Resource m
  }

makeClassy ''ResourceData

-- | Container used to keep track of the decision state and what is known
-- about response while processing a request.
data ReqData = ReqData
  { _reqDataRespMediaType :: MediaType
  , _reqDataRespCharset :: Maybe Charset
  , _reqDataRespEncoding :: Maybe Encoding
  , _reqDataDispPath :: [Text]
  , _reqDataRespHeaders :: HeadersMap
  , _reqDataRespBody :: Maybe Body
  }

makeClassy ''ReqData

