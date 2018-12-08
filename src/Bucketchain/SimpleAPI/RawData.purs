module Bucketchain.SimpleAPI.RawData where

import Bucketchain.Http (Http)

-- | A type synonym for raw data.
type RawData =
  { http :: Http
  , path :: String
  , rawBody :: String
  }
