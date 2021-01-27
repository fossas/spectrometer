{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Control.Effect.Record
  ( Recordable (..),
    RecordC (..),
  )
where

import Control.Algebra
import Control.Carrier.State.Strict
import Control.Effect.Sum
import Control.Monad.Trans
import Data.Aeson
import Data.Kind
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Unsafe.Coerce

class Recordable (r :: Type -> Type) where
  record :: r a -> a -> (Value, Value)

newtype RecordC (e :: (Type -> Type) -> Type -> Type) (sig :: (Type -> Type) -> Type -> Type) (m :: Type -> Type) a = RecordC
  { runRecordC :: StateC (Map Value Value) m a
  }
  deriving (Functor, Applicative, Monad, MonadTrans)

-- | We can handle an arbitrary effect 'e' -- @Algebra (e :+: sig) (RecordC e sig m)@
-- ..but we require a few things:
-- 1. 'e' must also appear somewhere else in the effect stack -- @Member e sig, Algebra sig m@
-- 2. 'e' is Recordable -- @Recordable (e m)@
--
-- There's a third claim we make, not reflected in the types: in the
-- instantiated effect type 'e m a', 'm' must be a phantom type variable. This
-- is reflected in our use of 'unsafeCoerce', and is required for us to 'send'
-- the effect further down the handler stack
instance (Member e sig, Algebra sig m, Recordable (e m)) => Algebra (e :+: sig) (RecordC e sig m) where
  -- The type signature is here to bring 'n' into scope for 'unsafeCoerce'.
  alg hdl sig' ctx = RecordC $ do
    case sig' of
      L eff -> do
        let eff' = unsafeCoerce eff :: e any a
        res <- lift $ send eff'

        let values = record eff' res
        modify (uncurry M.insert values)

        pure (res <$ ctx)
      R other -> alg (runRecordC . hdl) (R other) ctx
