module Bucketchain.SimpleAPI.Class where

import Prelude

import Bucketchain.SimpleAPI.Auth (Auth(..))
import Bucketchain.SimpleAPI.Auth.Class (class Authenticatable, authenticate)
import Bucketchain.SimpleAPI.Batch (BatchParams, Batch(..))
import Bucketchain.SimpleAPI.Body (Body(..), decodeBody)
import Bucketchain.SimpleAPI.FreeT.Class (class Transformable, transform)
import Bucketchain.SimpleAPI.Proc (Proc, context, runProc)
import Bucketchain.SimpleAPI.RawData (RawData)
import Bucketchain.SimpleAPI.Response (Response, fromResponses, invalidRequestResponse, unauthorizedResponse, errorResponse)
import Bucketchain.SimpleAPI.Response.Class (class Respondable, toResponse)
import Control.Monad.Free.Trans (FreeT, runFreeT)
import Control.Parallel (parTraverse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (drop)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect.Aff (Aff, attempt)
import Foreign (MultipleErrors)
import Prim.Row (class Cons)
import Record.Unsafe (unsafeGet)
import Simple.JSON (class ReadForeign, writeJSON)
import Type.RowList (class RowToList, kind RowList, Cons, Nil, RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | A typeclass what type is servable.
-- |
-- | If you return `Nothing` in implementation, SimpleAPI proceeds to the next middleware.
class Servable ex server where
  serve :: server -> ex -> RawData -> Aff (Maybe Response)

-- | This is for internal. Do not use it.
class ServableList ex (l :: RowList) (r :: # Type) | l -> r where
  serveList :: RLProxy l -> Record r -> ex -> RawData -> Aff (Maybe Response)

instance servableProc :: (Respondable a) => Servable ex (Proc ex a) where
  serve server extraData rawData = do
    result <- attempt $ runProc server $ context extraData rawData
    case result of
      Left _ -> pure $ Just errorResponse
      Right x -> pure $ Just $ toResponse x

instance servableFreeT :: (Transformable ex f, Respondable a) => Servable ex (FreeT f (Proc ex) a) where
  serve server extraData rawData = serve (runFreeT transform server) extraData rawData

instance servableWithBody :: (ReadForeign a, Servable ex server) => Servable ex (Body a -> server) where
  serve server extraData rawData =
    case decodeBody rawData.http rawData.rawBody of
      Left _ ->
        pure $ Just invalidRequestResponse
      Right body ->
        serve (server $ Body body) extraData rawData

instance servableWithAuth :: (Authenticatable ex a, Servable ex server) => Servable ex (Auth a -> server) where
  serve server extraData rawData = do
    result <- runProc authenticate $ context extraData rawData
    case result of
      Nothing -> pure $ Just unauthorizedResponse
      Just x ->
        serve (server $ Auth x) extraData rawData

instance servableBatch :: Servable ex server => Servable ex (Batch server) where
  serve (Batch server) extraData rawData =
    if not isBatch
      then serve server extraData rawData
      else
        case decoded of
          Left _ ->
            pure $ Just invalidRequestResponse
          Right bodies -> do
            Just <$> fromResponses <$> parTraverse serve' bodies
    where
      isBatch = rawData.path == "batch"

      decoded :: Either MultipleErrors BatchParams
      decoded = decodeBody rawData.http rawData.rawBody

      serve' { path, body } =
        serve server extraData
          rawData
            { path = drop 1 path
            , rawBody = writeJSON body
            }

instance servableRecord :: (RowToList r l, ServableList ex l r) => Servable ex (Record r) where
  serve = serveList (RLProxy :: RLProxy l)

instance servableListNil :: ServableList ex Nil () where
  serveList _ _ _ _ = pure Nothing

instance servableListCons
  :: (IsSymbol route, Servable ex server, ServableList ex taill tailr, Cons route server tailr r)
  => ServableList ex (Cons route server taill) r where
  serveList _ rec extraData rawData
    | rawData.path == reflectSymbol (SProxy :: SProxy route) = serve (unsafeGet rawData.path rec :: server) extraData rawData
    | otherwise = serveList (RLProxy :: RLProxy taill) (unsafeCoerce rec) extraData rawData
