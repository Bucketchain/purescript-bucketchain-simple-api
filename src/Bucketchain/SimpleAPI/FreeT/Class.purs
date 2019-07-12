module Bucketchain.SimpleAPI.FreeT.Class where

import Prelude

import Bucketchain.SimpleAPI.Action (Action)
import Control.Monad.Free.Trans (FreeT)

-- | A typeclass for `FreeT` request handlers.
-- |
-- | `ex` is any extra data. It is typically global context such as db connection and can be used in `Action`.
class (Functor f) <= Transformable ex f where
  transform :: forall a. f (FreeT f (Action ex) a) -> Action ex (FreeT f (Action ex) a)
