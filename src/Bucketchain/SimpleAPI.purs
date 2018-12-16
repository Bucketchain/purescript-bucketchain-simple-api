module Bucketchain.SimpleAPI where

import Prelude

import Bucketchain.Http (requestMethod, requestBody, requestURL, setStatusCode, setHeader)
import Bucketchain.Middleware (Middleware)
import Bucketchain.ResponseBody (body)
import Bucketchain.SimpleAPI.Class (class Servable, serve)
import Bucketchain.SimpleAPI.Response (Response(..))
import Control.Monad.Reader (ask)
import Data.Maybe (Maybe(..))
import Data.String (drop)
import Data.TraversableWithIndex (traverseWithIndex)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Simple.JSON (writeJSON)

-- | SimpleAPI middleware.
-- |
-- | `ex` is any extra data. It is typically global context such as db connection and can be used in `Action`.
-- |
-- | `server` is a `Servable` instance.
withSimpleAPI
  :: forall ex server
   . Servable ex server
  => ex
  -> server
  -> Middleware
withSimpleAPI extraData server next = do
  http <- ask
  if requestMethod http /= "POST"
    then next
    else do
      let path = drop 1 $ requestURL http
      rawBody <- liftAff $ requestBody http
      result <- liftAff $ serve server extraData { http, path, rawBody }
      case result of
        Nothing -> next
        Just (Response response) -> liftEffect do
          setStatusCode http response.status
          void $ traverseWithIndex (setHeader http) response.headers
          Just <$> (body $ writeJSON response.body)
