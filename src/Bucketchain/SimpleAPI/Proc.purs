module Bucketchain.SimpleAPI.Proc
  ( Context
  , Proc
  , askExtra
  , askRaw
  , runProc
  , context
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

-- | This is for internal. Do not use it.
newtype Context ex =
  Context
    { extraData :: ex
    , rawData :: RawData
    }

-- | The type of request handler.
newtype Proc ex a = Proc (ReaderT (Context ex) Aff a)

derive newtype instance functorProc :: Functor (Proc ex)
derive newtype instance applyProc :: Apply (Proc ex)
derive newtype instance applicativeProc :: Applicative (Proc ex)
derive newtype instance altProc :: Alt (Proc ex)
derive newtype instance plusProc :: Plus (Proc ex)
derive newtype instance bindProc :: Bind (Proc ex)
derive newtype instance monadProc :: Monad (Proc ex)
derive newtype instance semigroupProc :: Semigroup a => Semigroup (Proc ex a)
derive newtype instance monoidProc :: Monoid a => Monoid (Proc ex a)
derive newtype instance monadEffectProc :: MonadEffect (Proc ex)
derive newtype instance monadAffProc :: MonadAff (Proc ex)
derive newtype instance monadThrowProc :: MonadThrow Error (Proc ex)
derive newtype instance monadErrorProc :: MonadError Error (Proc ex)
derive newtype instance monadAskProc :: MonadAsk (Context ex) (Proc ex)
derive newtype instance monadRecProc :: MonadRec (Proc ex)

-- | Get global context.
askExtra :: forall ex. Proc ex ex
askExtra = flip map ask \(Context ctx) -> ctx.extraData

-- | Get `RawData`.
askRaw :: forall ex. Proc ex RawData
askRaw = flip map ask \(Context ctx) -> ctx.rawData

-- | This is for internal. Do not use it.
runProc :: forall ex a. Proc ex a -> Context ex -> Aff a
runProc (Proc readerT) = runReaderT readerT

-- | This is for internal. Do not use it.
context :: forall ex. ex -> RawData -> Context ex
context extraData rawData = Context { extraData, rawData }
