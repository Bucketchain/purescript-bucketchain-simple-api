module Bucketchain.SimpleAPI.Batch where

import Foreign (Foreign)

-- | This is for internal. Do not use it.
type BatchParams = Array
  { path :: String
  , body :: Foreign
  }

-- | The type for enable batch operation.
-- |
-- | The batch operation is a mechanism running multiple `Action` in one HTTP request.
-- |
-- | How to use:
-- |
-- | ```purescript
-- | withSimpleAPI appContext $ Batch { firstEndpoint, secondEndpoint }
-- | ```
-- |
-- | Request `POST /batch` with body:
-- |
-- | ```json
-- | [
-- |   { "path": "/firstEndpoint" },
-- |   { "path": "/secondEndpoint", "body": { "num": 1 } }
-- | ]
-- | ```
-- |
-- | You will get results like:
-- |
-- | ```json
-- | [
-- |   {
-- |     "status": 500,
-- |     "headers": { "content-type": "application/json; charset=utf-8" },
-- |     "body": { "message": "Internal server error" }
-- |   },
-- |   {
-- |     "status": 201,
-- |     "headers": { "content-type": "application/json; charset=utf-8" },
-- |     "body": { "name": "foo" }
-- |   }
-- | ]
-- | ```
-- |
-- | Note: `POST /batch` responds 200 always.
newtype Batch a = Batch a
