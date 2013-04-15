module Webcrank.Method where

import Data.Text

data Method =
    Options
  | Get
  | Head
  | Post
  | Put
  | Delete
  | Trace
  | Connect
  | ExtensionMethod Text
