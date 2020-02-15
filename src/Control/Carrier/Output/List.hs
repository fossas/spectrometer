module Control.Carrier.Output.List
  ( OutputC
  , runOutput

  , module X
  ) where

import Control.Monad.IO.Class
import Control.Effect.Output as X
import Control.Carrier.State.Strict
import Prelude

runOutput :: OutputC o m a -> m ([o], a)
runOutput = undefined

newtype OutputC o m a = OutputC { runOutputC :: StateC [o] m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, Effect sig) => Algebra (Output o :+: sig) (OutputC o m) where
  alg (R other) = alg (R (handleCoercible other))
  alg (L (Output o k)) = OutputC (modify (o:)) *> k
