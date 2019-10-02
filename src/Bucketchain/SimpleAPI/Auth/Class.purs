module Bucketchain.SimpleAPI.Auth.Class where

import Bucketchain.SimpleAPI.Proc (Proc)
import Data.Maybe (Maybe)

-- | A typeclass for authentication.
-- |
-- | `ex` is any extra data. It is typically global context such as db connection and can be used in `Proc`.
-- |
-- | `a` is a authenticated result. It is typically user data.
-- |
-- | If you return `Nothing` in `authenticate` implementation, a server responds 401.
class Authenticatable ex a where
  authenticate :: Proc ex (Maybe a)
