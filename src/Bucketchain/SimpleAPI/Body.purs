module Bucketchain.SimpleAPI.Body
  ( Body(..)
  , decodeBody
  ) where

import Prelude

import Bucketchain.Http (Http, requestHeaders)
import Data.Array (head)
import Data.Either (Either(..))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Data.String (Pattern(..), split)
import Foreign (ForeignError(..), MultipleErrors)
import Foreign.Object (lookup)
import Simple.JSON (class ReadForeign, readJSON)

-- | The type for request handlers with request body.
-- |
-- | You can use `ReadForeign` instances as type variable `a`.
newtype Body a = Body a

-- | This is for internal. Do not use it.
decodeBody :: forall a. ReadForeign a => Http -> String -> Either MultipleErrors a
decodeBody http rawBody =
  case lookup "content-type" (requestHeaders http) >>= parseMediaType of
    Just mediaType | mediaType == applicationJSON -> readJSON rawBody
    _ -> Left $ singleton $ ForeignError "Received unpermitted Content-Type."

parseMediaType :: String -> Maybe MediaType
parseMediaType = split (Pattern ";") >>> head >>> map MediaType
