module Effect.Logger
  ( Logger(..)
  , LogMsg(..)
  , Severity(..)

  , LoggerC(..)
  , IgnoreLoggerC(..)
  , runLogger
  , ignoreLogger

  , log
  , logSticky

  , logTrace
  , logDebug
  , logInfo
  , logWarn
  , logError

  , module Data.Text.Prettyprint.Doc
  , module Data.Text.Prettyprint.Doc.Render.Terminal
  ) where

import Prologue

import Control.Algebra
import Control.Carrier.Reader
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (atomically, check, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, tryReadTQueue, writeTQueue)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

data Logger m k
  = Log LogMsg (m k)
  deriving (Generic1)

data LogMsg
  = LogNormal Severity (Doc AnsiStyle)
  | LogSticky (Doc AnsiStyle)
  deriving (Show, Generic)

instance HFunctor Logger
instance Effect Logger

-- | Log a message with the given severity
log :: Has Logger sig m => Severity -> Doc AnsiStyle -> m ()
log severity logLine = send (Log (LogNormal severity logLine) (pure ()))

-- | Log a "sticky" line -- a log line that sticks to the bottom of the terminal until cleared or overwritten by other sticky line.
--
-- NOTE: The 'Doc' must not contain newlines
logSticky :: Has Logger sig m => Doc AnsiStyle -> m ()
logSticky logLine = send (Log (LogSticky logLine) (pure ()))

data Severity =
    SevTrace
  | SevDebug
  | SevInfo
  | SevWarn
  | SevError
  deriving (Eq, Ord, Show, Generic)

logTrace :: Has Logger sig m => Doc AnsiStyle -> m ()
logTrace = log SevTrace

logDebug :: Has Logger sig m => Doc AnsiStyle -> m ()
logDebug = log SevDebug

logInfo :: Has Logger sig m => Doc AnsiStyle -> m ()
logInfo = log SevInfo

logWarn :: Has Logger sig m => Doc AnsiStyle -> m ()
logWarn = log SevWarn

logError :: Has Logger sig m => Doc AnsiStyle -> m ()
logError = log SevError

runLogger :: MonadIO m => Severity -> LoggerC m a -> m a
runLogger minSeverity (LoggerC act) = do
  queue <- liftIO (hSetBuffering stdout NoBuffering *> newTQueueIO @LogMsg)
  cancelVar <- liftIO (newTVarIO False)

  let loop :: Text -> IO ()
      loop sticky = do
        maybeMsg <- atomically $ do
          msg <- tryReadTQueue queue
          case msg of
            Just a -> pure (Just a)
            Nothing -> do
              canceled <- readTVar cancelVar
              check canceled
              pure Nothing

        case maybeMsg of
          Nothing -> pure ()
          Just msg -> do
            let stickyLen   = T.length sticky
                clearSticky = TIO.putStr (T.replicate stickyLen "\b" <> T.replicate stickyLen " " <> T.replicate stickyLen "\b")
            case msg of
              LogNormal sev logLine -> do
                when (sev >= minSeverity) $ do
                  clearSticky
                  printIt $ logLine <> line
                  TIO.putStr sticky
                loop sticky
              LogSticky logLine -> do
                clearSticky
                let rendered = renderIt logLine
                TIO.putStr rendered
                loop rendered

  res <- runReader queue act

  tid <- liftIO $ async $ loop ""
  liftIO $ do
    atomically $ writeTVar cancelVar True
    void (wait tid)

  pure res

newtype LoggerC m a = LoggerC { runLoggerC :: ReaderC (TQueue LogMsg) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (Logger :+: sig) (LoggerC m) where
  alg (L (Log msg k)) = do
    queue <- LoggerC ask
    liftIO $ atomically $ writeTQueue queue msg
    k
   
  alg (R other) = LoggerC (alg (R (handleCoercible other)))

newtype IgnoreLoggerC m a = IgnoreLoggerC { runIgnoreLoggerC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

ignoreLogger :: IgnoreLoggerC m a -> m a
ignoreLogger = runIgnoreLoggerC

instance (Algebra sig m, Effect sig) => Algebra (Logger :+: sig) (IgnoreLoggerC m) where
  alg (L (Log _ k)) = k
  alg (R other) = IgnoreLoggerC (alg (handleCoercible other))

printIt :: Doc AnsiStyle -> IO ()
printIt = renderIO stdout . layoutPretty defaultLayoutOptions

renderIt :: Doc AnsiStyle -> Text
renderIt = renderStrict . layoutPretty defaultLayoutOptions
