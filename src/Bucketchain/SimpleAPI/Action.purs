module Bucketchain.SimpleAPI.Action
  ( Context(..)
  , Action
  , askExtra
  , askRaw
  , runAction
  ) where

import Prelude

import Bucketchain.SimpleAPI.RawData (RawData)
import Control.Alt (class Alt)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Plus (class Plus)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)

-- | The type of global context.
newtype Context ex =
  Context
    { extraData :: ex
    , rawData :: RawData
    }

-- | The type of request handler.
newtype Action ex a = Action (ReaderT (Context ex) Aff a)

derive newtype instance functorAction :: Functor (Action ex)
derive newtype instance applyAction :: Apply (Action ex)
derive newtype instance applicativeAction :: Applicative (Action ex)
derive newtype instance altAction :: Alt (Action ex)
derive newtype instance plusAction :: Plus (Action ex)
derive newtype instance bindAction :: Bind (Action ex)
derive newtype instance monadAction :: Monad (Action ex)
derive newtype instance semigroupAction :: Semigroup a => Semigroup (Action ex a)
derive newtype instance monoidAction :: Monoid a => Semigroup (Action ex a)
derive newtype instance monadEffectAction :: MonadEffect (Action ex)
derive newtype instance monadAffAction :: MonadAff (Action ex)
derive newtype instance monadThrowAction :: MonadThrow Error (Action ex)
derive newtype instance monadErrorAction :: MonadError Error (Action ex)
derive newtype instance monadAskAction :: MonadAsk (Context ex) (Action ex)
derive newtype instance monadRecAction :: MonadRec (Action ex)

-- | Get global context.
askExtra :: forall ex. Action ex ex
askExtra = flip map ask \(Context ctx) -> ctx.extraData

-- | Get `RawData`.
askRaw :: forall ex. Action ex RawData
askRaw = flip map ask \(Context ctx) -> ctx.rawData

-- | This is for internal. Do not use it.
runAction :: forall ex a. Action ex a -> Context ex -> Aff a
runAction (Action readerT) = runReaderT readerT
