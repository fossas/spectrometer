{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Control.Carrier.Output.Interpret
  ( OutputC
  , runOutput
  ) where

import Control.Applicative (Alternative)
import Control.Carrier.Reader
import Control.Effect.Output as X
import Control.Monad.IO.Class
import Control.Monad.Trans (MonadTrans(..))

newtype OutputC o m a = OutputC {runOutputC :: ReaderC (o -> OutputC o m ()) m a}
  deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadFail)

instance MonadTrans (OutputC o) where
  lift = OutputC . lift

runOutput :: (o -> OutputC o m ()) -> OutputC o m a -> m a
runOutput f = runReader f . runOutputC

instance Algebra sig m => Algebra (Output o :+: sig) (OutputC o m) where
  alg hdl sig ctx = OutputC $ case sig of
    L (Output o) -> do
      fn <- ask @(o -> OutputC o m ())
      runOutputC (fn o)
      pure ctx
    R other -> alg (runOutputC . hdl) (R other) ctx
