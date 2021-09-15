{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Fossa.Analyze.Debug (
  -- * Debug individual effects
  DiagDebugC,
  diagToDebug,
  ReadFSDebugC,
  readFSToDebug,
  ExecDebugC,
  execToDebug,
  LogDebugC,
  logToDebug,

  -- * Debug all effects
  debugEverything,
) where

import Control.Carrier.Debug
import Control.Carrier.Diagnostics (Diagnostics (Context, Fatal))
import Control.Carrier.Simple (SimpleC, interpret)
import Control.Effect.Sum (Member, inj)
import Control.Monad.IO.Class (MonadIO)
import Effect.Exec (Exec, ExecF (..))
import Effect.Logger (Logger, LoggerF (..))
import Effect.ReadFS (ReadFS, ReadFSF (..))

newtype DiagDebugC m a = DiagDebugC {runDiagDebugC :: m a}
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Transcribe 'context' as 'debugScope', and 'fatal' as 'debugError'
instance (Member Diagnostics sig, Member Debug sig, Algebra sig m) => Algebra (Diagnostics :+: sig) (DiagDebugC m) where
  alg hdl sig ctx = DiagDebugC $
    case sig of
      L thing@(Context nm _) -> debugScope nm $ alg (runDiagDebugC . hdl) (inj thing) ctx
      L thing@(Fatal err) -> do
        debugError err
        alg (runDiagDebugC . hdl) (inj thing) ctx
      L ours -> alg (runDiagDebugC . hdl) (inj ours) ctx
      R other -> alg (runDiagDebugC . hdl) other ctx

diagToDebug :: DiagDebugC m a -> m a
diagToDebug = runDiagDebugC

-----------------------------------------------

type ReadFSDebugC = SimpleC ReadFSF

-- | Record most ReadFS constructors, ignoring ListDir because it explodes the
-- size of the debug bundle
readFSToDebug :: (Has ReadFS sig m, Has Debug sig m) => ReadFSDebugC m a -> m a
readFSToDebug = interpret $ \case
  cons@ReadContentsBS'{} -> recording cons
  cons@ReadContentsText'{} -> recording cons
  cons@DoesFileExist{} -> recording cons
  cons@DoesDirExist{} -> recording cons
  cons@ResolveFile'{} -> recording cons
  cons@ResolveDir'{} -> recording cons
  cons@ListDir{} -> ignoring cons

-----------------------------------------------

type ExecDebugC = SimpleC ExecF

execToDebug :: (Has Exec sig m, Has Debug sig m) => ExecDebugC m a -> m a
execToDebug = interpret $ \case
  cons@Exec{} -> recording cons

-----------------------------------------------

type LogDebugC = SimpleC LoggerF

logToDebug :: (Has Logger sig m, Has Debug sig m) => LogDebugC m a -> m a
logToDebug = interpret $ \case
  cons@Log{} -> recording cons
  cons@LogStdout{} -> ignoring cons

-----------------------------------------------

-- | Combine all of our debug wrappers into a mega-wrapper
type DebugEverythingC m = DiagDebugC (ReadFSDebugC (ExecDebugC m))

debugEverything :: (Has Debug sig m, Has Exec sig m, Has ReadFS sig m, Has Logger sig m) => DebugEverythingC m a -> m a
debugEverything = execToDebug . readFSToDebug . diagToDebug
