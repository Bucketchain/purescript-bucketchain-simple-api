module Bucketchain.SimpleAPI where

import Prelude

import Bucketchain.Http (requestMethod, requestBody, requestURL, setStatusCode, setHeader)
import Bucketchain.Middleware (Middleware)
import Bucketchain.ResponseBody (body)
import Bucketchain.SimpleAPI.Class (class Servable, serve)
import Bucketchain.SimpleAPI.Response (responseStatus, responseBody)
import Control.Monad.Reader (ask)
import Data.Maybe (Maybe(..))
import Data.String (drop)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Simple.JSON (writeJSON)

-- | SimpleAPI middleware.
-- |
-- | `ex` is any extra data. It is typically global context such as db connection and can be used in `Proc`.
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
        Just r -> liftEffect do
          setStatusCode http $ responseStatus r
          setHeader http "Content-Type" "application/json; charset=utf-8"
          Just <$> (body $ writeJSON $ responseBody r)
