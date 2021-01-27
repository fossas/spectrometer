{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Control.Effect.Record
  ( Recordable (..),
    RecordableValue (..),
    RecordC (..),
    runRecord,
  )
where

import Control.Algebra
import Control.Carrier.State.Strict
import Control.Effect.Sum
import Control.Monad.Trans
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Kind
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LEncoding
import Unsafe.Coerce
import Path

class Recordable (r :: Type -> Type) where
  record :: r a -> a -> (Value, Value)

runRecord :: RecordC e sig m a -> m (Map Value Value, a)
runRecord = runState M.empty . runRecordC

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

class RecordableValue a where
  toRecordedValue :: a -> Value
  default toRecordedValue :: ToJSON a => a -> Value
  toRecordedValue = toJSON

----- Stock RecordableValue instances

instance RecordableValue ()

instance RecordableValue Bool

instance RecordableValue Char

instance RecordableValue Double

instance RecordableValue Float

instance RecordableValue Int

instance RecordableValue Integer

instance RecordableValue LText.Text

instance RecordableValue Text.Text

instance RecordableValue Value

----- Composite instances

instance RecordableValue a => RecordableValue (Maybe a) where
  toRecordedValue (Just a) = toRecordedValue a
  toRecordedValue Nothing = Null

instance {-# OVERLAPPABLE #-} RecordableValue a => RecordableValue [a] where
  toRecordedValue = toJSON . map toRecordedValue

instance {-# OVERLAPPING #-} RecordableValue [Char]

instance (RecordableValue a, RecordableValue b) => RecordableValue (Either a b) where
  toRecordedValue (Left a) = object ["Left" .= toRecordedValue a]
  toRecordedValue (Right b) = object ["Right" .= toRecordedValue b]

instance (RecordableValue a, RecordableValue b) => RecordableValue (a, b) where
  toRecordedValue (a,b) = toJSON [toRecordedValue a, toRecordedValue b]

instance (RecordableValue a, RecordableValue b, RecordableValue c) => RecordableValue (a, b, c) where
  toRecordedValue (a,b,c) = toJSON [toRecordedValue a, toRecordedValue b, toRecordedValue c]

instance (RecordableValue a, RecordableValue b, RecordableValue c, RecordableValue d) => RecordableValue (a, b, c, d) where
  toRecordedValue (a,b,c,d) = toJSON [toRecordedValue a, toRecordedValue b, toRecordedValue c, toRecordedValue d]

instance (RecordableValue a, RecordableValue b, RecordableValue c, RecordableValue d, RecordableValue e) => RecordableValue (a, b, c, d, e) where
  toRecordedValue (a,b,c,d,e) = toJSON [toRecordedValue a, toRecordedValue b, toRecordedValue c, toRecordedValue d, toRecordedValue e]

instance (RecordableValue a, RecordableValue b, RecordableValue c, RecordableValue d, RecordableValue e, RecordableValue f) => RecordableValue (a, b, c, d, e, f) where
  toRecordedValue (a,b,c,d,e,f) = toJSON [toRecordedValue a, toRecordedValue b, toRecordedValue c, toRecordedValue d, toRecordedValue e, toRecordedValue f]

instance (RecordableValue a, RecordableValue b, RecordableValue c, RecordableValue d, RecordableValue e, RecordableValue f, RecordableValue g) => RecordableValue (a, b, c, d, e, f, g) where
  toRecordedValue (a,b,c,d,e,f,g) = toJSON [toRecordedValue a, toRecordedValue b, toRecordedValue c, toRecordedValue d, toRecordedValue e, toRecordedValue f, toRecordedValue g]

instance (RecordableValue a, RecordableValue b, RecordableValue c, RecordableValue d, RecordableValue e, RecordableValue f, RecordableValue g, RecordableValue h) => RecordableValue (a, b, c, d, e, f, g, h) where
  toRecordedValue (a,b,c,d,e,f,g,h) = toJSON [toRecordedValue a, toRecordedValue b, toRecordedValue c, toRecordedValue d, toRecordedValue e, toRecordedValue f, toRecordedValue g, toRecordedValue h]

----- Additional instances

instance RecordableValue BS.ByteString where
  toRecordedValue = toJSON . Encoding.decodeUtf8

instance RecordableValue BL.ByteString where
  toRecordedValue = toJSON . LEncoding.decodeUtf8

instance RecordableValue (Path a b)
