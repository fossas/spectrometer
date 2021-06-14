{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Simple (
  SimpleC (..),
  interpret,
  interpretState,
  module X,
) where

import Control.Algebra as X
import Control.Applicative
import Control.Carrier.Reader
import Control.Monad.Trans
import Data.Kind (Type)
import Unsafe.Coerce (unsafeCoerce)
import Control.Carrier.State.Strict

newtype SimpleC (eff :: (Type -> Type) -> Type -> Type) m a = SimpleC {runSimpleC :: ReaderC (HandlerFor eff m) m a}
  deriving (Functor, Applicative, Alternative, Monad, MonadFail, MonadIO)

withReaderC :: (r' -> r) -> ReaderC r m a -> ReaderC r' m a
withReaderC f act = ReaderC $ \r -> runReader (f r) act

hoistReaderC :: (forall x. m x -> n x) -> ReaderC r m a -> ReaderC r n a
hoistReaderC f = undefined

instance MonadTrans (SimpleC eff) where
  lift = SimpleC . lift

interpret :: (forall x. eff Unused x -> m x) -> SimpleC eff m a -> m a
interpret f = runReader (HandlerFor f) . runSimpleC

interpretState :: Monad m => s -> (forall a. eff Unused a -> StateC s m a) -> SimpleC eff m b -> m (s,b)
interpretState s f = undefined -- runState s . interpret f . hoistSimple lift

data Unused (a :: Type)

data HandlerFor (eff :: (Type -> Type) -> Type -> Type) m where
  HandlerFor :: (eff Unused a -> m a) -> HandlerFor eff m

hoistHandler :: (forall x. m x -> n x) -> HandlerFor eff m -> HandlerFor eff n
hoistHandler f (HandlerFor g) = HandlerFor (f . g)

-- HandlerFor eff m -> m a

instance Algebra sig m => Algebra (eff :+: sig) (SimpleC eff m) where
  alg hdl sig ctx = SimpleC $ do
    case sig of
      L ours -> do
        HandlerFor g <- ask @(HandlerFor eff m)
        res <- lift $ g (unsafeCoerce ours)
        pure (unsafeCoerce res <$ ctx)
      R other -> alg (runSimpleC . hdl) (R other) ctx
