module Main where

import Prelude

import Bucketchain (createServer, listen)
import Bucketchain.Http (requestHeaders)
import Bucketchain.Middleware (Middleware)
import Bucketchain.SimpleAPI (withSimpleAPI)
import Bucketchain.SimpleAPI.Auth (Auth(..))
import Bucketchain.SimpleAPI.Auth.Class (class Authenticatable)
import Bucketchain.SimpleAPI.Batch (Batch(..))
import Bucketchain.SimpleAPI.Body (Body(..))
import Bucketchain.SimpleAPI.FreeT.Class (class Transformable)
import Bucketchain.SimpleAPI.JSON (JSON, failure, success, success_)
import Bucketchain.SimpleAPI.Proc (Proc, askExtra, askRaw)
import Bucketchain.SimpleAPI.RawData (RawData)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Free.Trans (FreeT, liftFreeT)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (error)
import Foreign.Object (lookup, empty, singleton)
import Node.HTTP (ListenOptions, Server)

type Item =
  { id :: Int
  , name :: String
  , num :: Int
  , path :: String
  , rawBody :: String
  }

type OtherItem =
  { name :: String
  }

newtype User = User { name :: String }

instance authenticatableUser :: Authenticatable Int User where
  authenticate = do
    { http } <- askRaw
    pure $ User <$> { name: _ } <$> (lookup "x-test-auth" $ requestHeaders http)

main :: Effect Unit
main = server >>= listen opts

server :: Effect Server
server = createServer middleware

opts :: ListenOptions
opts =
  { hostname: "127.0.0.1"
  , port: 3000
  , backlog: Nothing
  }

middleware :: Middleware
middleware = withSimpleAPI 777 $ Batch
  { successTest
  , failureTest
  , bodyTest
  , authTest
  , errorTest
  , freeTTest
  }

successTest :: Proc Int (JSON (Array Item))
successTest = do
  num <- askExtra
  { path, rawBody } <- askRaw
  pure $ success headers 200 [ { id: 1, name: "Item 1", num, path, rawBody } ]
  where
    headers = singleton "X-Custom" "CustomValue"

failureTest :: Proc Int (JSON Item)
failureTest = pure $ failure headers 503 [ "This is error test" ]
  where
    headers = singleton "X-Custom" "CustomValue2"

bodyTest :: Body OtherItem -> Proc Int (JSON OtherItem)
bodyTest (Body x) = pure $ success empty 201 x

authTest :: Auth User -> Proc Int (JSON OtherItem)
authTest (Auth (User x)) = pure $ success empty 200 { name: x.name }

errorTest :: Proc Int (JSON OtherItem)
errorTest = throwError $ error "Test error"

freeTTest :: VProc (JSON OtherItem)
freeTTest = do
  num <- getExtra
  rawData <- getRawData
  pure $ success_ 200 { name: show num <> rawData.rawBody }

-- FreeT Example
data VProcF a
  = GetExtra (Int -> a)
  | GetRawData (RawData -> a)

type VProc = FreeT VProcF (Proc Int)

derive instance functorVProc :: Functor VProcF

getExtra :: VProc Int
getExtra = liftFreeT $ GetExtra identity

getRawData :: VProc RawData
getRawData = liftFreeT $ GetRawData identity

instance transformableVProcF :: Transformable Int VProcF where
  transform (GetExtra k) = k <$> askExtra
  transform (GetRawData k) = k <$> askRaw
