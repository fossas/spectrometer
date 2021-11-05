{-# LANGUAGE GADTs #-}

module Control.Effect.TaskPool (
  TaskPool (..),
  forkTask,
  module X,
) where

import Control.Algebra as X (
  Algebra (alg),
  Handler,
  Has,
  run,
  send,
  thread,
  (~<~),
  type (:+:) (L, R),
 )

data TaskPool m k where
  ForkTask :: m a -> TaskPool m ()

forkTask :: Has TaskPool sig m => m a -> m ()
forkTask act = send (ForkTask act)
