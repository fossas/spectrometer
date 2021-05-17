{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Effect.Logger
  ( Logger (..),
    LogAction (..),
    determineDefaultLogAction,
    LogMsg (..),
    Severity (..),
    LoggerC (..),
    IgnoreLoggerC (..),
    withLogger,
    withDefaultLogger,
    runLogger,
    ignoreLogger,
    log,
    logDebug,
    logInfo,
    logWarn,
    logError,
    logStdout,
    module X,
  )
where

import Control.Algebra as X
import Control.Applicative (Alternative)
import Control.Carrier.Reader
import Control.Effect.Exception
import Control.Effect.Lift (sendIO)
import Control.Effect.ConsoleRegion
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc as X
import Data.Text.Prettyprint.Doc.Render.Terminal as X
import System.Console.Concurrent
import System.IO (stdout)
import Prelude hiding (log)
import System.Console.ANSI (hSupportsANSI)
import Control.Monad.Trans (lift)

newtype LogAction m = LogAction
  { unLogAction :: LogMsg -> m ()
  }

data Logger (m :: Type -> Type) k where
  Log :: LogMsg -> Logger m ()

data LogMsg
  = LogNormal Severity (Doc AnsiStyle)
  | LogStdout (Doc AnsiStyle)
  deriving (Show)

-- | Log a message with the given severity
log :: Has Logger sig m => Severity -> Doc AnsiStyle -> m ()
log severity logLine = send (Log (LogNormal severity logLine))

-- | Log a line to stdout. Usually, you want to use 'log', 'logInfo', ..., instead
logStdout :: Has Logger sig m => Doc AnsiStyle -> m ()
logStdout logLine = send (Log (LogStdout logLine))

data Severity
  = SevDebug
  | SevInfo
  | SevWarn
  | SevError
  deriving (Eq, Ord, Show)

logDebug :: Has Logger sig m => Doc AnsiStyle -> m ()
logDebug = log SevDebug

logInfo :: Has Logger sig m => Doc AnsiStyle -> m ()
logInfo = log SevInfo

logWarn :: Has Logger sig m => Doc AnsiStyle -> m ()
logWarn = log SevWarn

logError :: Has Logger sig m => Doc AnsiStyle -> m ()
logError = log SevError

withLogger :: Has (Lift IO) sig m => LogAction m -> LoggerC m a -> m a
withLogger act = displayConsoleRegions . runLogger act

withDefaultLogger :: Has (Lift IO) sig m => Severity -> LoggerC m a -> m a
withDefaultLogger sev act = do
  logAction <- determineDefaultLogAction sev
  withLogger logAction act

runLogger :: LogAction m -> LoggerC m a -> m a
runLogger act = runReader act . runLoggerC

-- | Determine the default LogAction to use by checking whether the terminal
-- supports ANSI rendering
determineDefaultLogAction :: Has (Lift IO) sig m => Severity -> m (LogAction m)
determineDefaultLogAction sev = do
  -- TODO: should we check stderr or stdout here?
  ansiSupported <- sendIO $ hSupportsANSI stdout
  if ansiSupported
    then pure (termLoggerAction sev)
    else pure (rawLoggerAction sev)

rawLoggerAction :: Has (Lift IO) sig m => Severity -> LogAction m
rawLoggerAction minSeverity = LogAction $ \case
  LogNormal sev logLine -> do
    let rendered = renderIt . unAnnotate $ logLine <> line
    when (sev >= minSeverity) $
      sendIO $ errorConcurrent rendered
  LogStdout logLine -> do
    let rendered = renderIt . unAnnotate $ logLine <> line
    sendIO $ outputConcurrent rendered

termLoggerAction :: Has (Lift IO) sig m => Severity -> LogAction m
termLoggerAction minSeverity = LogAction $ \case
  LogNormal sev logLine ->
    when (sev >= minSeverity) $
      sendIO $ errorConcurrent $ renderIt $ logLine <> line
  LogStdout logLine ->
    sendIO $ outputConcurrent . renderIt $ logLine <> line

newtype LoggerC m a = LoggerC {runLoggerC :: ReaderC (LogAction m) m a}
  deriving (Functor, Applicative, Alternative, Monad, MonadIO)

instance (Algebra sig m, Has (Lift IO) sig m) => Algebra (Logger :+: sig) (LoggerC m) where
  alg hdl sig ctx = LoggerC $ case sig of
    L (Log msg) -> do
      LogAction action <- ask @(LogAction m)
      lift $ action msg
      pure ctx
    R other -> alg (runLoggerC . hdl) (R other) ctx

newtype IgnoreLoggerC m a = IgnoreLoggerC {runIgnoreLoggerC :: m a}
  deriving (Functor, Applicative, Monad, MonadIO)

ignoreLogger :: IgnoreLoggerC m a -> m a
ignoreLogger = runIgnoreLoggerC

instance Algebra sig m => Algebra (Logger :+: sig) (IgnoreLoggerC m) where
  alg hdl sig ctx = IgnoreLoggerC $ case sig of
    L (Log _) -> pure ctx
    R other -> alg (runIgnoreLoggerC . hdl) other ctx

renderIt :: Doc AnsiStyle -> Text
renderIt = renderStrict . layoutPretty defaultLayoutOptions
