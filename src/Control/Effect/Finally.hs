{-# LANGUAGE GADTs #-}

module Control.Effect.Finally (
  -- * Finally effect
  Finally (..),
  onExit,

  -- * Re-exports
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

data Finally m k where
  OnExit :: m a -> Finally m ()

onExit :: Has Finally sig m => m a -> m ()
onExit = send . OnExit
