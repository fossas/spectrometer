module Control.Effect.Debug (
  Debug (..),
  debugScope,
  debugMetadata,
  debugError,
  debugEffect,
  module X,
) where

import Control.Algebra as X
import Control.Effect.Diagnostics (ToDiagnostic)
import Control.Effect.Record (Recordable)
import Data.Text (Text)
import Data.Aeson (ToJSON)

data Debug m a where
  DebugScope :: Text -> m a -> Debug m a
  DebugMetadata :: ToJSON a => Text -> a -> Debug m ()
  DebugError :: ToDiagnostic err => err -> Debug m ()
  DebugEffect :: Recordable r => r a -> a -> Debug m ()

debugScope :: Has Debug sig m => Text -> m a -> m a
debugScope nm act = send (DebugScope nm act)

-- | Add a key/value pair to the top-level scope. When using an overlapping key,
-- only the latest value is used
debugMetadata :: (ToJSON a, Has Debug sig m) => Text -> a -> m ()
debugMetadata key val = send (DebugMetadata key val)

debugError :: (ToDiagnostic err, Has Debug sig m) => err -> m ()
debugError = send . DebugError

debugEffect :: (Recordable r, Has Debug sig m) => r a -> a -> m ()
debugEffect k v = send (DebugEffect k v)
