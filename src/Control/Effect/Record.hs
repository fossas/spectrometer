{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Control.Effect.Record (
  Recordable (..),
  RecordableValue (..),
  RecordC (..),
  runRecord,
  Journal (..),
  EffectResult (..),
) where

import Control.Algebra
import Control.Carrier.AtomicState
import Control.Carrier.Simple
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Monad.Trans
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Kind
import Data.String.Conversion (decodeUtf8)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Path
import System.Exit
import Data.Void (Void)
import Unsafe.Coerce (unsafeCoerce)

-- | A class of "recordable" effects -- i.e. an effect whose data constructors
-- and "result values" (the @a@ in @e m a@) can be serialized to JSON values
class (forall a. Ord (r a)) => Recordable (r :: Type -> Type) where
  -- | Serialize a data constructor to JSON
  recordKey :: r a -> Value

  -- | Serialize an effect data constructor's "return value" to JSON
  recordValue :: r a -> a -> Value

-- | A journal contains all of the effect invocations recorded by RecordC
newtype Journal eff = Journal {unJournal :: Map Value Value}
  deriving (Eq, Ord, Show)

-- | The result of an effectful action
data EffectResult r where
  EffectResult :: r a -> a -> EffectResult r

instance FromJSON (Journal eff) where
  parseJSON = fmap (Journal . Map.fromList) . parseJSON

instance ToJSON (Journal eff) where
  toJSON = toJSON . Map.toList . unJournal

-- | Wrap and record an effect; generally used with @-XTypeApplications@, e.g.,
--
-- > runRecord @SomeEffect
runRecord :: forall e sig m a. (Recordable e, Has (Lift IO) sig m) => RecordC e sig m a -> m (Journal e, a)
runRecord act = do
  (mapping, a) <- runAtomicState Map.empty . runRecordC $ act
  pure (convertToJournal (Map.elems mapping), a)

convertToJournal :: Recordable e => [EffectResult e] -> Journal e
convertToJournal = Journal . Map.fromList . map (\(EffectResult k v) -> (recordKey k, recordValue k v))

-- | @RecordC e sig m a@ is a pseudo-carrier for an effect @e@ with the underlying signature @sig@
newtype RecordC (e :: Type -> Type) (sig :: (Type -> Type) -> Type -> Type) (m :: Type -> Type) a = RecordC
  { runRecordC :: AtomicStateC (Map (e Void) (EffectResult e))  m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

-- | We can handle a simple effect 'e' -- @Algebra (Simple e :+: sig) (RecordC e sig m)@ ..but we require a few things:
--
-- 1. 'e' must also appear somewhere else in the effect stack -- @Member (Simple e) sig@
-- 2. 'e' is Recordable -- @Recordable e@
instance (Member (Simple e) sig, Has (Lift IO) sig m, Recordable e) => Algebra (Simple e :+: sig) (RecordC e sig m) where
  alg hdl sig' ctx = RecordC $ do
    case sig' of
      L (Simple eff) -> do
        res <- lift $ send (Simple eff)

        modify @(Map (e Void) (EffectResult e)) (insertUnlessExists (unsafeCoerce eff) (EffectResult eff res))

        pure (res <$ ctx)
      R other -> alg (runRecordC . hdl) (R other) ctx

insertUnlessExists :: Ord k => k -> v -> Map k v -> Map k v
insertUnlessExists k v m
  | Map.member k m = m
  | otherwise = Map.insert k v m

-- | RecordableValue is essentially @ToJSON@ with a different name. We use
-- RecordableValue to avoid orphan ToJSON instances for, e.g., ByteString and
-- ExitCode
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
  toRecordedValue (a, b) = toJSON [toRecordedValue a, toRecordedValue b]

instance (RecordableValue a, RecordableValue b, RecordableValue c) => RecordableValue (a, b, c) where
  toRecordedValue (a, b, c) = toJSON [toRecordedValue a, toRecordedValue b, toRecordedValue c]

instance (RecordableValue a, RecordableValue b, RecordableValue c, RecordableValue d) => RecordableValue (a, b, c, d) where
  toRecordedValue (a, b, c, d) = toJSON [toRecordedValue a, toRecordedValue b, toRecordedValue c, toRecordedValue d]

instance (RecordableValue a, RecordableValue b, RecordableValue c, RecordableValue d, RecordableValue e) => RecordableValue (a, b, c, d, e) where
  toRecordedValue (a, b, c, d, e) = toJSON [toRecordedValue a, toRecordedValue b, toRecordedValue c, toRecordedValue d, toRecordedValue e]

instance (RecordableValue a, RecordableValue b, RecordableValue c, RecordableValue d, RecordableValue e, RecordableValue f) => RecordableValue (a, b, c, d, e, f) where
  toRecordedValue (a, b, c, d, e, f) = toJSON [toRecordedValue a, toRecordedValue b, toRecordedValue c, toRecordedValue d, toRecordedValue e, toRecordedValue f]

instance (RecordableValue a, RecordableValue b, RecordableValue c, RecordableValue d, RecordableValue e, RecordableValue f, RecordableValue g) => RecordableValue (a, b, c, d, e, f, g) where
  toRecordedValue (a, b, c, d, e, f, g) = toJSON [toRecordedValue a, toRecordedValue b, toRecordedValue c, toRecordedValue d, toRecordedValue e, toRecordedValue f, toRecordedValue g]

instance (RecordableValue a, RecordableValue b, RecordableValue c, RecordableValue d, RecordableValue e, RecordableValue f, RecordableValue g, RecordableValue h) => RecordableValue (a, b, c, d, e, f, g, h) where
  toRecordedValue (a, b, c, d, e, f, g, h) = toJSON [toRecordedValue a, toRecordedValue b, toRecordedValue c, toRecordedValue d, toRecordedValue e, toRecordedValue f, toRecordedValue g, toRecordedValue h]

----- Additional instances

instance RecordableValue BS.ByteString where
  toRecordedValue = toJSON . decodeUtf8 @Text.Text

instance RecordableValue BL.ByteString where
  toRecordedValue = toJSON . decodeUtf8 @LText.Text

instance RecordableValue (Path a b)

instance RecordableValue (SomeBase a)

instance RecordableValue ExitCode where
  toRecordedValue ExitSuccess = toJSON (0 :: Int)
  toRecordedValue (ExitFailure i) = toJSON (i :: Int)
