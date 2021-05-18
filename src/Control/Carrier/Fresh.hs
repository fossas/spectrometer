{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Fresh (
  FreshC,
  runFresh,

  -- * Re-exports
  module X,
) where

import Control.Carrier.AtomicState
import Control.Effect.Fresh as X
import Control.Effect.Lift
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans)

runFresh :: Has (Lift IO) sig m => FreshC m a -> m a
runFresh = fmap snd . runAtomicState 1 . runFreshC

newtype FreshC m a = FreshC {runFreshC :: AtomicStateC Int m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance Has (Lift IO) sig m => Algebra (Fresh :+: sig) (FreshC m) where
  alg hdl sig ctx = FreshC $ case sig of
    L GenerateId -> do
      generated <- getSet @Int (\old -> (old+1, old))
      pure (generated <$ ctx)
    R other -> alg (runFreshC . hdl) (R other) ctx
