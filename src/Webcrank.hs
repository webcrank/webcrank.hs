{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Webcrank
  ( -- * Resources
    Resource(..)
  , resource
  , resourceWithBody
  , resourceWithHtml
  , Encoding
  , Authorized(..)
  , ETag(..)
  , PostAction(..)
  , postCreate
  , postCreateRedir
  , postProcess
  , postProcessRedir
  , HaltT
  , halt
  , werror
    -- * Charsets
  , Charset
  , CharsetsProvided(..)
  , provideCharsets
    -- * Headers
  , HeadersMap
  , addResponseHeader
  , putResponseHeader
    -- * Body
  , Body
  , textBody
  , lazyTextBody
  , strBody
  , writeLBS
  , writeStr
    -- * Extra convience (re)exports
  , module Network.HTTP.Date
  , module Network.HTTP.Media
  , module Network.HTTP.Types
  , hAcceptCharset
  , hAcceptEncoding
  , hAllow
  , hETag
  , hExpires
  , hIfMatch
  , hIfNoneMatch
  , hIfUnmodifiedSince
  , hTransferEncoding
  , hVary
  , hWWWAuthenticate
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.List.NonEmpty
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Network.HTTP.Date
import Network.HTTP.Media
import Network.HTTP.Types

import Webcrank.Internal.Halt
import Webcrank.Internal.Headers
import Webcrank.Internal.ReqData
import Webcrank.Internal.Types

-- | Builds a @Resource m@ value where all the resource functions will
-- return default values as described in the @'Resource'@ function
-- documentation.
resource :: Monad m => Resource m
resource = Resource
  { serviceAvailable = return True
  , uriTooLong = return False
  , allowedMethods = return [methodGet, methodHead]
  , malformedRequest = return False
  , isAuthorized = return Authorized
  , forbidden = return False
  , validContentHeaders = return True
  , knownContentType = return True
  , validEntityLength = return True
  , options = return []
  , contentTypesProvided = return []
  , charsetsProvided = return NoCharset
  , encodingsProvided = return []
  , resourceExists = return True
  , generateETag = mzero
  , lastModified = mzero
  , expires = mzero
  , movedPermanently = mzero
  , movedTemporarily = mzero
  , previouslyExisted = return False
  , allowMissingPost = return False
  , deleteResource = return False
  , deleteCompleted = return True
  , postAction = postProcess $ return ()
  , contentTypesAccepted = return []
  , variances = return []
  , multipleChoices = return False
  , isConflict = return False
  , finishRequest = return ()
  }

-- | Creates a resource that provides a single content type.
resourceWithBody :: Monad m => MediaType -> m Body -> Resource m
resourceWithBody t b = resource { contentTypesProvided = return [(t, lift b)] }

-- | Creates a resource that provides a @text/html@ content type.
resourceWithHtml :: Monad m => m Body -> Resource m
resourceWithHtml b = resourceWithBody "text/html" b

-- | Shortcut for @return . CharsetsProvided@
provideCharsets
  :: Monad m
  => NonEmpty (Charset, Body -> Body)
  -> m CharsetsProvided
provideCharsets = return . CharsetsProvided

-- | Add a header to the response.
addResponseHeader
  :: (MonadState s m, HasReqData s)
  => HeaderName
  -> ByteString
  -> m ()
addResponseHeader h v = reqDataRespHeaders %= HashMap.insertWith (<>) h [v]

-- | Create a @PostAction@ which performs resource creation without redirecting.
postCreate :: Monad m => [Text] -> m (PostAction m)
postCreate = return . PostCreate
{-# INLINE postCreate #-}

-- | Create a @PostAction@ which performs resource creation and redirects.
postCreateRedir :: Monad m => [Text] -> m (PostAction m)
postCreateRedir = return . PostCreateRedir
{-# INLINE postCreateRedir #-}

-- | Create a @PostAction@ which runs some process and does not redirect.
postProcess :: Monad m => HaltT m () -> m (PostAction m)
postProcess = return . PostProcess
{-# INLINE postProcess #-}

-- | Create a @PostAction@ which runs some process and does redirects.
postProcessRedir :: Monad m => HaltT m ByteString -> m (PostAction m)
postProcessRedir = return . PostProcessRedir
{-# INLINE postProcessRedir #-}

-- | Create a response @Body@ from strict @Text@.
textBody :: Text -> Body
textBody = LB.fromStrict . T.encodeUtf8
{-# INLINE textBody #-}

-- | Create a response @Body@ from lazy @Text@.
lazyTextBody :: LT.Text -> Body
lazyTextBody = LT.encodeUtf8
{-# INLINE lazyTextBody #-}

-- | Create a response @Body@ from a @String@.
strBody :: String -> Body
strBody = lazyTextBody . LT.pack
{-# INLINE strBody #-}

-- | Set the response body from a @String@
writeStr
  :: (MonadState s m, HasReqData s)
  => String
  -> m ()
writeStr = assign reqDataRespBody . Just . strBody
{-# INLINE writeStr #-}

