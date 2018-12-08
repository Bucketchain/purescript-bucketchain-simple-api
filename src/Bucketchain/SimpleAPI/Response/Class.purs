module Bucketchain.SimpleAPI.Response.Class where

import Bucketchain.SimpleAPI.Response (Response)

-- | A typeclass what type can be response.
class Respondable a where
  toResponse :: a -> Response
