module Bucketchain.SimpleAPI.Response
  ( Response
  , StatusCode
  , response
  , responseStatus
  , responseBody
  , fromResponses
  , invalidRequestResponse
  , unauthorizedResponse
  ) where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Data.Nullable (Nullable, null)
import Foreign (Foreign)
import Simple.JSON (class WriteForeign, write)

type StatusCode = Int

-- | The type for response used in `Servable` implementation.
newtype Response = Response
  { status :: StatusCode
  , body :: Foreign
  }

derive newtype instance writeForeignResponse :: WriteForeign Response

-- | Constructor function of `Response`.
response :: StatusCode -> Foreign -> Response
response status body = Response { status, body }

-- | This is for internal. Do not use it.
responseStatus :: Response -> StatusCode
responseStatus (Response { status }) = status

-- | This is for internal. Do not use it.
responseBody :: Response -> Foreign
responseBody (Response { body }) = body

-- | This is for internal. Do not use it.
fromResponses :: Array (Maybe Response) -> Response
fromResponses xs = response 200 body
  where
    notFound = response 404 $ write (null :: Nullable Foreign)
    body = write $ fromMaybe notFound <$> xs

-- | This is for internal. Do not use it.
invalidRequestResponse :: Response
invalidRequestResponse = response 400 $ write { message: "Request body is invalid" }

-- | This is for internal. Do not use it.
unauthorizedResponse :: Response
unauthorizedResponse = response 401 $ write { message: "Unauthorized" }
