module Bucketchain.SimpleAPI.JSON
  ( JSON
  , FailureMessages
  , success
  , failure
  , success_
  , failure_
  ) where

import Prelude

import Bucketchain.SimpleAPI.Response (Headers, StatusCode, response)
import Bucketchain.SimpleAPI.Response.Class (class Respondable)
import Foreign.Object (Object, insert, empty)
import Simple.JSON (class WriteForeign, write)

type FailureMessages = Object (Array String)

-- | The type for JSON response.
data JSON a
  = Success
      { headers :: Headers
      , status :: StatusCode
      , body :: a
      }
  | Failure
      { headers :: Headers
      , status :: StatusCode
      , body :: FailureMessages
      }

instance respondableJSON :: (WriteForeign a) => Respondable (JSON a) where
  toResponse (Success x) =
    response (withJSONContentType x.headers) x.status $ write x.body
  toResponse (Failure x) =
    response (withJSONContentType x.headers) x.status $ write x.body

-- | Create a success response.
success :: forall a. WriteForeign a => Headers -> StatusCode -> a -> JSON a
success headers status body = Success { headers, status, body }

-- | Create a failure response.
failure :: forall a. WriteForeign a => Headers -> StatusCode -> FailureMessages -> JSON a
failure headers status body = Failure { headers, status, body }

-- | Create a success response without headers.
success_ :: forall a. WriteForeign a => StatusCode -> a -> JSON a
success_ status body = Success { headers: empty, status, body }

-- | Create a failure response without headers.
failure_ :: forall a. WriteForeign a => StatusCode -> FailureMessages -> JSON a
failure_ status body = Failure { headers: empty, status, body }

withJSONContentType :: Headers -> Headers
withJSONContentType = insert "Content-Type" "application/json; charset=utf-8"
