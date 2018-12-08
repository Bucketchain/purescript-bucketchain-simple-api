module Main where

import Prelude

import Bucketchain (createServer, listen)
import Bucketchain.Http (requestHeaders)
import Bucketchain.Middleware (Middleware)
import Bucketchain.SimpleAPI (withSimpleAPI)
import Bucketchain.SimpleAPI.Action (Action, askExtra, askRaw)
import Bucketchain.SimpleAPI.Auth (Auth(..))
import Bucketchain.SimpleAPI.Auth.Class (class Authenticatable)
import Bucketchain.SimpleAPI.Batch (Batch(..))
import Bucketchain.SimpleAPI.Body (Body(..))
import Bucketchain.SimpleAPI.JSON (JSON, success, failure)
import Control.Monad.Error.Class (throwError)
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
    case lookup "x-test-auth" $ requestHeaders http of
      Nothing -> throwError $ error "Test error"
      Just x -> pure $ User { name: x }

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
middleware = withSimpleAPI 777 $ Batch { successTest, failureTest, bodyTest, authTest, errorTest }

successTest :: Action Int (JSON (Array Item))
successTest = do
  num <- askExtra
  { path, rawBody } <- askRaw
  pure $ success headers 200 [{ id: 1, name: "Item 1", num, path, rawBody }]
  where
    headers = singleton "X-Custom" "CustomValue"

failureTest :: Action Int (JSON Item)
failureTest = pure $ failure headers 503 $ singleton "core" ["This is error test"]
  where
    headers = singleton "X-Custom" "CustomValue2"

bodyTest :: Body OtherItem -> Action Int (JSON OtherItem)
bodyTest (Body x) = pure $ success empty 201 x

authTest :: Auth User -> Action Int (JSON OtherItem)
authTest (Auth (User x)) = pure $ success empty 200 { name: x.name }

errorTest :: Action Int (JSON OtherItem)
errorTest = throwError $ error "Test error"
