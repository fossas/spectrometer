{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Fossa.Analyze.Debug (
  collectDebugBundle,

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
import Control.Carrier.Lift (sendIO)
import Control.Carrier.Simple (SimpleC, interpret)
import Control.Effect.Lift (Lift)
import Control.Effect.Sum (Member, inj)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (ToJSON)
import Data.Word (Word64)
import Effect.Exec (Exec, ExecF (..))
import Effect.Logger (Logger, LoggerF (..))
import Effect.ReadFS (ReadFS, ReadFSF (..))
import GHC.Conc qualified as Conc
import GHC.Environment qualified as Environment
import GHC.Generics (Generic)
import GHC.Stats qualified as Stats
import System.Info qualified as Info

collectDebugBundle ::
  ( Has Exec sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) =>
  DebugEverythingC (DebugC m) a ->
  m (DebugBundle, a)
collectDebugBundle act = do
  (scope, res) <- runDebug . debugEverything $ act
  sysInfo <- sendIO collectSystemInfo
  args <- sendIO Environment.getFullArgs
  let bundle =
        DebugBundle
          { bundleScope = scope
          , bundleSystem = sysInfo
          , bundleArgs = args
          }
  pure (bundle, res)

data DebugBundle = DebugBundle
  { bundleScope :: Scope
  , bundleSystem :: SystemInfo
  , bundleArgs :: [String]
  }
  deriving (Show, Generic)

instance ToJSON DebugBundle

collectSystemInfo :: IO SystemInfo
collectSystemInfo = do
  capabilities <- Conc.getNumCapabilities
  processors <- Conc.getNumProcessors
  systemMemory <- do
    rtsStatsEnabled <- Stats.getRTSStatsEnabled
    if rtsStatsEnabled
      then do
        rtsStats <- Stats.getRTSStats
        pure (SystemMemory (Stats.max_live_bytes rtsStats) (Stats.allocated_bytes rtsStats))
      else pure (SystemMemory 0 0)
  pure $
    SystemInfo
      Info.os
      Info.arch
      capabilities
      processors
      systemMemory

data SystemInfo = SystemInfo
  { systemInfoOs :: String
  , systemInfoArch :: String
  , systemCapabilities :: Int
  , systemProcessors :: Int
  , systemMemory :: SystemMemory
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SystemInfo

data SystemMemory = SystemMemory
  { systemMemoryLiveBytes :: Word64
  , systemMemoryAllocatedBytes :: Word64
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON SystemMemory

-----------------------------------------------

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

debugEverything :: (Has Debug sig m, Has Exec sig m, Has ReadFS sig m) => DebugEverythingC m a -> m a
debugEverything = execToDebug . readFSToDebug . diagToDebug
