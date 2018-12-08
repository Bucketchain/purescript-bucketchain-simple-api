module Bucketchain.SimpleAPI.Auth.Class where

import Bucketchain.SimpleAPI.Action (Action)

-- | A typeclass for authentication.
-- |
-- | `ex` is any extra data. It is typically global context such as db connection and can be used in `Action`.
-- |
-- | `a` is a authenticated result. It is typically user data.
-- |
-- | If you call `throwError` in `authenticate` implementation, a server responds 401.
class Authenticatable ex a where
  authenticate :: Action ex a
