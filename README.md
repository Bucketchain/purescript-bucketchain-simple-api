# purescript-bucketchain-simple-api

[![Latest release](http://img.shields.io/github/release/Bucketchain/purescript-bucketchain-simple-api.svg)](https://github.com/Bucketchain/purescript-bucketchain-simple-api/releases)

A simple RPC style API middleware of [Bucketchain](https://github.com/Bucketchain/purescript-bucketchain).

See [example](https://github.com/Bucketchain/purescript-bucketchain-simple-api/blob/master/example/Main.purs).

## Installation

### Bower

```
$ bower install purescript-bucketchain-simple-api
```

### Spago

```
$ spago install bucketchain-simple-api
```

## Getting Started

```purescript
import Prelude

import App.DB (Pool, createConnectionPool, withPool, selectItems, createItem)
import Bucketchain (createServer, listen)
import Bucketchain.Middleware (Middleware)
import Bucketchain.SimpleAPI (withSimpleAPI)
import Bucketchain.SimpleAPI.Body (Body(..))
import Bucketchain.SimpleAPI.JSON (JSON, success)
import Bucketchain.SimpleAPI.Proc (Proc, askExtra)
import Effect (Effect)
import Node.HTTP (ListenOptions, Server)

type Item =
  { id :: Int
  , name :: String
  }

type ItemParams =
  { name :: String
  }

main :: Effect Unit
main = do
  pool <- createConnectionPool
  server pool >>= listen opts

opts :: ListenOptions
opts =
  { hostname: "127.0.0.1"
  , port: 3000
  , backlog: Nothing
  }

-- Initialize SimpleAPI.

server :: Pool -> Effect Server
server = createServer $ withSimpleAPI pool routes
  where
    -- SimpleAPI is simple RPC style. it's POST only.
    -- POST /getItems
    -- POST /createItem
    routes = { getItems, createItem }

-- Define endpoints.

getItems :: Proc Pool (JSON (Array Item))
getItems = do
  pool <- askExtra
  items <- liftAff $ withPool selectItems pool
  pure $ success 200 items

createItem :: Body ItemParams -> Proc Pool (JSON Item)
createItem (Body params) = do
  pool <- askExtra
  item <- liftAff $ withPool (createItem params) pool
  pure $ success 201 item
```

## Enable the batch operation

You can wrap routes with `Batch`:


```purescript
import Bucketchain.SimpleAPI.Batch (Batch(..))

server :: Pool -> Effect Server
server = createServer $ withSimpleAPI pool $ Batch routes
  where
    -- POST /getItems
    -- POST /createItem
    routes = { getItems, createItem }

```

Request `POST /batch` with body:

```json
[
  { "path": "/getItems" },
  { "path": "/createItem", "body": { "name": "Item 3" } }
]
```

You will get results like:

```json
[
  {
    "status": 200,
    "body": [ { "id": 1, "name": "Item 1" }, { "id": 2, "name": "Item 2" } ]
  },
  {
    "status": 201,
    "body": { "id": 3, "name": "Item 3" }
  }
]
```

Note: `POST /batch` responds 200 always.

## Authentication

You can use `Authenticatable` for authentication.

```purescript
import Bucketchain.SimpleAPI.Auth.Class (class Authenticatable)
import Bucketchain.SimpleAPI.Proc (askExtra, askRaw)

newtype User = User { id :: Int, name :: String }

instance authenticatableUser :: Authenticatable Pool User where
  authenticate = do
    { http } <- askRaw
    case lookup "authorization" $ requestHeaders http of
      Nothing -> pure Nothing
      Just x -> do
        pool <- askExtra
        liftAff $ Just <$> withPool (selectUserFromToken x) pool
```

Then, define request handlers with `Auth`.

```purescript
getItems :: Auth User -> Proc Pool (JSON (Array Item))
getItems (Auth user) = -- ...
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-bucketchain-simple-api).

## LICENSE

MIT
