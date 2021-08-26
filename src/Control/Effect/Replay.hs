{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Replay (
  Replayable (..),
  ReplayableValue (..),
  runReplay,
) where

import Control.Algebra
import Control.Applicative
import Control.Carrier.AtomicState
import Control.Carrier.Reader
import Control.Carrier.Simple
import Control.Effect.Lift
import Control.Effect.Record
import Control.Effect.Sum
import Control.Monad.Trans
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.Kind
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as MMap
import Data.String.Conversion (encodeUtf8, toString)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Text.Lazy qualified as TL
import Path
import System.Exit
import Unsafe.Coerce

-- | A class of "replayable" effects -- i.e. an effect whose "result values"
-- (the @a@ in @e m a@) can be deserialized from JSON values produced by
-- 'recordValue' from 'Recordable'
class (Recordable r, forall a. Ord (r a)) => Replayable (r :: Type -> Type) where
  -- | Deserialize an effect data constructor's "return value" from JSON
  replay :: r a -> Value -> Maybe a

data Any

newtype ReplayC (e :: Type -> Type) (sig :: (Type -> Type) -> Type -> Type) (m :: Type -> Type) a = ReplayC
  { runReplayC :: ReaderC (HashMap Value Value) (AtomicStateC (Map (e Any) Some) m) a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (ReplayC e sig) where
  lift = ReplayC . lift . lift

-- | Wrap an effect carrier, and replay its effects given the log produced by
-- 'runRecord'. If a log entry isn't available for a given effect invocation, we
-- pass the effect call down to the wrapped carrier
runReplay :: forall e sig m a. Has (Lift IO) sig m => Journal e -> ReplayC e sig m a -> m a
runReplay (Journal mapping) = fmap snd . runAtomicState MMap.empty . runReader mapping . runReplayC

data Some where
  Some :: a -> Some

instance (Member (Simple e) sig, Has (Lift IO) sig m, Replayable e) => Algebra (Simple e :+: sig) (ReplayC e sig m) where
  alg hdl sig' ctx = ReplayC $ do
    case sig' of
      L (Simple eff) -> do
        mapping <- ask @(HashMap Value Value)
        memo <- get @(Map (e Any) Some)
        let keyVal = recordKey eff
        case MMap.lookup (unsafeCoerce eff) memo of
          Just (Some val) -> do
            pure (unsafeCoerce val <$ ctx)
          Nothing -> do
            case Map.lookup keyVal mapping >>= replay eff of
              Nothing -> do
                -- TODO: log warnings on key miss
                res <- lift $ send (Simple eff)
                modify @(Map (e Any) Some) (MMap.insert (unsafeCoerce eff) (Some res))
                pure (res <$ ctx)
              Just result -> do
                modify @(Map (e Any) Some) (MMap.insert (unsafeCoerce eff) (Some result))
                pure (result <$ ctx)
      R other -> alg (runReplayC . hdl) (R (R other)) ctx

-- | ReplayableValue is essentially @FromJSON@ with a different name. We use
-- ReplayableValue to avoid orphan FromJSON instances for, e.g., ByteString and
-- ExitCode
class ReplayableValue a where
  fromRecordedValue :: Value -> Parser a
  default fromRecordedValue :: FromJSON a => Value -> Parser a
  fromRecordedValue = parseJSON

----- Stock ReplayableValue instances

instance ReplayableValue ()

instance ReplayableValue Bool

instance ReplayableValue Char

instance ReplayableValue Double

instance ReplayableValue Float

instance ReplayableValue Int

instance ReplayableValue Integer

instance ReplayableValue LText.Text

instance ReplayableValue Text.Text

instance ReplayableValue Value

----- Composite instances

instance ReplayableValue a => ReplayableValue (Maybe a) where
  fromRecordedValue Null = pure Nothing
  fromRecordedValue x = Just <$> fromRecordedValue x

instance {-# OVERLAPPABLE #-} ReplayableValue a => ReplayableValue [a] where
  fromRecordedValue val = do
    xs <- parseJSON val
    traverse fromRecordedValue xs

instance {-# OVERLAPPING #-} ReplayableValue [Char] where
  fromRecordedValue = withText "String" (pure . toString)

instance (ReplayableValue a, ReplayableValue b) => ReplayableValue (Either a b) where
  fromRecordedValue = withObject "Either" $ \obj -> do
    (Left <$> (obj .: "Left" >>= fromRecordedValue)) <|> (Right <$> (obj .: "Right" >>= fromRecordedValue))

instance (ReplayableValue a, ReplayableValue b) => ReplayableValue (a, b) where
  fromRecordedValue val = do
    [a, b] <- fromRecordedValue val
    (,) <$> fromRecordedValue a <*> fromRecordedValue b

instance (ReplayableValue a, ReplayableValue b, ReplayableValue c) => ReplayableValue (a, b, c) where
  fromRecordedValue val = do
    [a, b, c] <- fromRecordedValue val
    (,,) <$> fromRecordedValue a <*> fromRecordedValue b <*> fromRecordedValue c

instance (ReplayableValue a, ReplayableValue b, ReplayableValue c, ReplayableValue d) => ReplayableValue (a, b, c, d) where
  fromRecordedValue val = do
    [a, b, c, d] <- fromRecordedValue val
    (,,,) <$> fromRecordedValue a <*> fromRecordedValue b <*> fromRecordedValue c <*> fromRecordedValue d

instance (ReplayableValue a, ReplayableValue b, ReplayableValue c, ReplayableValue d, ReplayableValue e) => ReplayableValue (a, b, c, d, e) where
  fromRecordedValue val = do
    [a, b, c, d, e] <- fromRecordedValue val
    (,,,,) <$> fromRecordedValue a <*> fromRecordedValue b <*> fromRecordedValue c <*> fromRecordedValue d <*> fromRecordedValue e

instance (ReplayableValue a, ReplayableValue b, ReplayableValue c, ReplayableValue d, ReplayableValue e, ReplayableValue f) => ReplayableValue (a, b, c, d, e, f) where
  fromRecordedValue val = do
    [a, b, c, d, e, f] <- fromRecordedValue val
    (,,,,,) <$> fromRecordedValue a <*> fromRecordedValue b <*> fromRecordedValue c <*> fromRecordedValue d <*> fromRecordedValue e <*> fromRecordedValue f

instance (ReplayableValue a, ReplayableValue b, ReplayableValue c, ReplayableValue d, ReplayableValue e, ReplayableValue f, ReplayableValue g) => ReplayableValue (a, b, c, d, e, f, g) where
  fromRecordedValue val = do
    [a, b, c, d, e, f, g] <- fromRecordedValue val
    (,,,,,,) <$> fromRecordedValue a <*> fromRecordedValue b <*> fromRecordedValue c <*> fromRecordedValue d <*> fromRecordedValue e <*> fromRecordedValue f <*> fromRecordedValue g

instance (ReplayableValue a, ReplayableValue b, ReplayableValue c, ReplayableValue d, ReplayableValue e, ReplayableValue f, ReplayableValue g, ReplayableValue h) => ReplayableValue (a, b, c, d, e, f, g, h) where
  fromRecordedValue val = do
    [a, b, c, d, e, f, g, h] <- fromRecordedValue val
    (,,,,,,,) <$> fromRecordedValue a <*> fromRecordedValue b <*> fromRecordedValue c <*> fromRecordedValue d <*> fromRecordedValue e <*> fromRecordedValue f <*> fromRecordedValue g <*> fromRecordedValue h

----- Additional instances

instance ReplayableValue BS.ByteString where
  fromRecordedValue = fmap (encodeUtf8 @Text.Text) . parseJSON

instance ReplayableValue BL.ByteString where
  fromRecordedValue = fmap (encodeUtf8 @TL.Text) . parseJSON

instance ReplayableValue (Path Abs Dir)
instance ReplayableValue (Path Abs File)
instance ReplayableValue (Path Rel Dir)
instance ReplayableValue (Path Rel File)

instance ReplayableValue (SomeBase Dir)
instance ReplayableValue (SomeBase File)

instance ReplayableValue ExitCode where
  fromRecordedValue val = do
    i <- parseJSON val
    case i of
      0 -> pure ExitSuccess
      _ -> pure $ ExitFailure i
