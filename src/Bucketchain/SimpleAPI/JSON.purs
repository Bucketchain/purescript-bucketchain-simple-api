module Bucketchain.SimpleAPI.JSON
  ( JSON
  , FailureMessages
  , success
  , failure
  ) where

import Prelude

import Bucketchain.SimpleAPI.Response (StatusCode, response)
import Bucketchain.SimpleAPI.Response.Class (class Respondable)
import Simple.JSON (class WriteForeign, write)

type FailureMessages = Array String

-- | The type for JSON response.
data JSON a
  = Success
      { status :: StatusCode
      , body :: a
      }
  | Failure
      { status :: StatusCode
      , body :: FailureMessages
      }

instance respondableJSON :: (WriteForeign a) => Respondable (JSON a) where
  toResponse (Success x) =
    response x.status $ write x.body
  toResponse (Failure x) =
    response x.status $ write x.body

-- | Create a success response.
success :: forall a. WriteForeign a => StatusCode -> a -> JSON a
success status body = Success { status, body }

-- | Create a failure response.
failure :: forall a. WriteForeign a => StatusCode -> FailureMessages -> JSON a
failure status body = Failure { status, body }
