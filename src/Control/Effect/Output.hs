{-# LANGUAGE GADTs #-}

module Control.Effect.Output (
  SOutput (..),
  Output,
  output,
  module X,
) where

import Control.Algebra as X
import Control.Carrier.Simple (Simple, sendSimple)

data SOutput o a where
  Output :: o -> SOutput o ()

type Output o = Simple (SOutput o)

output :: Has (Output o) sig m => o -> m ()
output o = sendSimple (Output o)
