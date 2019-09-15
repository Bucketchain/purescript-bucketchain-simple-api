module Bucketchain.SimpleAPI.FreeT.Class where

import Prelude

import Bucketchain.SimpleAPI.Proc (Proc)
import Control.Monad.Free.Trans (FreeT)

-- | A typeclass for `FreeT` request handlers.
-- |
-- | `ex` is any extra data. It is typically global context such as db connection and can be used in `Proc`.
class (Functor f) <= Transformable ex f where
  transform :: forall a. f (FreeT f (Proc ex) a) -> Proc ex (FreeT f (Proc ex) a)
