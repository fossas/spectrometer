module Control.Effect.Debug (
  Debug (..),
  debugScope,
  debugError,
  debugEffect,
  debugFile,
  debugBuildtool,
  module X,
) where

import Control.Algebra as X
import Control.Effect.Diagnostics (ToDiagnostic)
import Control.Effect.Record (Recordable)
import Data.Text (Text)
import Effect.Exec (Command)
import Path

data Debug m a where
  DebugScope :: Text -> m a -> Debug m a
  DebugError :: ToDiagnostic err => err -> Debug m ()
  DebugEffect :: Recordable r => r a -> a -> Debug m ()
  DebugFile :: Path Abs File -> Debug m ()
  DebugBuildtool :: Command -> Debug m ()

debugScope :: Has Debug sig m => Text -> m a -> m a
debugScope nm act = send (DebugScope nm act)

debugError :: (ToDiagnostic err, Has Debug sig m) => err -> m ()
debugError = send . DebugError

debugEffect :: (Recordable r, Has Debug sig m) => r a -> a -> m ()
debugEffect k v = send (DebugEffect k v)

debugFile :: Has Debug sig m => Path Abs File -> m ()
debugFile = send . DebugFile

debugBuildtool :: Has Debug sig m => Command -> m ()
debugBuildtool = send . DebugBuildtool
