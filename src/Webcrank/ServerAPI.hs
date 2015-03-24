module Webcrank.ServerAPI
  ( ServerAPI(..)
  , ReqData
  , newReqData
  , HasReqData(reqData)
  , ResourceData
  , newResourceData
  , HasResourceData(resourceData)
  , LogData
  , handleRequest
  ) where

import Webcrank.Internal.HandleRequest
import Webcrank.Internal.ReqData
import Webcrank.Internal.ResourceData
import Webcrank.Internal.Types

