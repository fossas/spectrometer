{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Finally (
  -- * Finally carrier
  FinallyC (..),
  runFinally,

  -- * Re-exports
  module X,
) where

import Control.Carrier.Reader (
  Algebra,
  Has,
  ReaderC,
  ask,
  run,
  runReader,
 )
import Control.Effect.Exception (finally)
import Control.Effect.Finally as X (
  Algebra (alg),
  Finally (OnExit),
  Handler,
  Has,
  onExit,
  run,
  send,
  thread,
  (~<~),
  type (:+:) (L, R),
 )
import Control.Effect.Lift (Lift, sendIO)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.IORef (
  IORef,
  atomicModifyIORef',
  newIORef,
  readIORef,
 )

newtype FinallyC m a = FinallyC {runFinallyC :: ReaderC (IORef [FinallyC m ()]) m a}
  deriving (Functor, Applicative, Monad, MonadIO)

runFinally :: Has (Lift IO) sig m => FinallyC m a -> m a
runFinally (FinallyC go) = do
  ref <- sendIO $ newIORef []
  runReader ref go `finally` (traverse_ runFinally =<< sendIO (readIORef ref))

instance (Has (Lift IO) sig m, Algebra sig m) => Algebra (Finally :+: sig) (FinallyC m) where
  alg hdl sig ctx = FinallyC $ case sig of
    L (OnExit go) -> do
      ref <- ask
      sendIO $ atomicModifyIORef' ref (\cur -> (void (hdl (go <$ ctx)) : cur, ()))
      pure ctx
    R other -> alg (runFinallyC . hdl) (R other) ctx
