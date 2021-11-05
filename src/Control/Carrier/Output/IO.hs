{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Output.IO (
  OutputC,
  runOutput,
  module X,
) where

import Control.Carrier.Reader (Algebra, Has, run)
import Control.Carrier.Simple (
  Handler,
  SimpleC,
  interpret,
  send,
  thread,
  (~<~),
 )
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Output as X (
  Algebra (alg),
  Handler,
  Has,
  Output,
  OutputF (Output),
  output,
  run,
  send,
  thread,
  (~<~),
  type (:+:) (L, R),
 )
import Data.IORef (
  IORef,
  atomicModifyIORef',
  newIORef,
  readIORef,
 )

type OutputC o = SimpleC (OutputF o)

runOutput :: forall o sig m a. Has (Lift IO) sig m => OutputC o m a -> m ([o], a)
runOutput act = do
  ref <- sendIO $ newIORef []
  res <- runOutputRef ref act
  outputs <- sendIO $ readIORef ref
  pure (reverse outputs, res)

runOutputRef :: Has (Lift IO) sig m => IORef [o] -> OutputC o m a -> m a
runOutputRef ref = interpret $ \case
  Output o -> sendIO (atomicModifyIORef' ref (\xs -> (o : xs, ())))
