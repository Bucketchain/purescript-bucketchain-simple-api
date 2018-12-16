module Bucketchain.SimpleAPI.Response
  ( Response
  , Headers
  , StatusCode
  , response
  , responseHeaders
  , responseStatus
  , responseBody
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

-- | Constructor function of `Response`.
response :: Headers -> StatusCode -> Foreign -> Response
response headers status body = Response { headers, status, body }

-- | This is for internal. Do not use it.
responseHeaders :: Response -> Headers
responseHeaders (Response { headers }) = headers

-- | This is for internal. Do not use it.
responseStatus :: Response -> StatusCode
responseStatus (Response { status }) = status

-- | This is for internal. Do not use it.
responseBody :: Response -> Foreign
responseBody (Response { body }) = body

-- | This is for internal. Do not use it.
fromResponses :: Array (Maybe Response) -> Response
fromResponses xs = response defaultHeaders 200 body
  where
    notFound = response defaultHeaders 404 $ write (null :: Nullable Foreign)
    body = write $ fromMaybe notFound <$> xs

-- | This is for internal. Do not use it.
invalidRequestResponse :: Response
invalidRequestResponse = response defaultHeaders 400 $ write { message: "Request body is invalid" }

-- | This is for internal. Do not use it.
unauthorizedResponse :: Response
unauthorizedResponse = response defaultHeaders 401 $ write { message: "Unauthorized" }

-- | This is for internal. Do not use it.
errorResponse :: Response
errorResponse = response defaultHeaders 500 $ write { message: "Internal server error" }

defaultHeaders :: Headers
defaultHeaders = singleton "Content-Type" "application/json; charset=utf-8"
