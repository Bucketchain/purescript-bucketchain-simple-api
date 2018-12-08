module Bucketchain.SimpleAPI.Response
  ( Response(..)
  , Headers
  , StatusCode
  , fromResponses
  , invalidRequestResponse
  , unauthorizedResponse
  , errorResponse
  ) where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Data.Nullable (Nullable, null)
import Foreign (Foreign)
import Foreign.Object (Object, singleton)
import Simple.JSON (class WriteForeign, write)

type Headers = Object String

type StatusCode = Int

-- | The type for response used in `Servable` implementation.
newtype Response = Response
  { headers :: Headers
  , status :: StatusCode
  , body :: Foreign
  }

derive newtype instance writeForeignResponse :: WriteForeign Response

-- | This is for internal. Do not use it.
fromResponses :: Array (Maybe Response) -> Response
fromResponses xs = Response { headers, status, body }
  where
    headers = singleton "Content-Type" "application/json; charset=utf-8"
    status = 200
    notFound = Response
      { headers
      , status: 404
      , body: write (null :: Nullable Foreign)
      }
    body = write $ fromMaybe notFound <$> xs

-- | This is for internal. Do not use it.
invalidRequestResponse :: Response
invalidRequestResponse =
  Response
    { headers: singleton "content-type" "application/json; charset=utf-8"
    , status: 400
    , body: write { message: "Request body is invalid" }
    }

-- | This is for internal. Do not use it.
unauthorizedResponse :: Response
unauthorizedResponse =
  Response
    { headers: singleton "content-type" "application/json; charset=utf-8"
    , status: 401
    , body: write { message: "Unauthorized" }
    }

-- | This is for internal. Do not use it.
errorResponse :: Response
errorResponse =
  Response
    { headers: singleton "content-type" "application/json; charset=utf-8"
    , status: 500
    , body: write { message: "Internal server error" }
    }
