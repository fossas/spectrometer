{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Control.Effect.Record
  ( Recordable (..),
    RecordC(..),
  )
where

import Control.Carrier.Reader
import Control.Monad.Trans
import Data.Aeson
import Data.Kind
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Effect.Sum
import Control.Carrier.Interpret
import Control.Algebra
import Data.Coerce
import Unsafe.Coerce

class Recordable r where
  record :: r a -> a -> (Value, Value)

newtype RecordC (e :: (Type -> Type) -> Type -> Type) (sig :: (Type -> Type) -> Type -> Type) (m :: Type -> Type) a = RecordC {runRecordC :: ReaderC (Map Value Value) m a}
  deriving (Functor, Applicative, Monad, MonadTrans)

--runRecord :: forall eff sig m n a. (Monad m, Member eff sig, Algebra sig n) => (forall s. Reifies s (Interpreter eff m) => InterpretC s eff m a) -> m a
--runRecord = runInterpret $ \_ r ctx -> do
  --res <- send (unsafeCoerce @(eff n _) r)
  --undefined

instance (Member e sig, Algebra sig m, Recordable (e m)) => Algebra (e :+: sig) (RecordC e sig m) where
  --alg hdl sig' ctx = case sig' of
    --L something -> do
      --res <- lift $ send (unsafeCoerce something)
      --send something
      --undefined
    --R something -> undefined
  alg :: Functor ctx => Handler ctx n (RecordC e sig m) -> (e :+: sig) n a -> ctx () -> RecordC e sig m (ctx a)
  alg hdl sig' ctx = RecordC $ do
    --mapping <- ask @(Map Value Value)
    case sig' of
      L something -> do
        let key = unsafeCoerce something :: e n a
        res <- lift $ send @e @sig key
        --lift $ send @e something
        --res' <- alg (runRecordC . hdl) (R (inj something)) ctx
        let ok = record key res
        pure (res <$ ctx)
      R other -> alg (runRecordC . hdl) (R other) ctx

--data Foo (m :: Type -> Type) k where
--Foo :: String -> Bool -> Foo m Int
--Bar :: Int -> Foo m String
--Baz :: Foo m ()

--instance Recordable (Foo m) where
--record r a = case r of
--Foo s b -> (toJSON ("foo" :: String, s, b), toJSON a)
--Bar i -> (toJSON ("bar" :: String, i), toJSON a)
--Baz -> (toJSON ("baz" :: String), toJSON a)
