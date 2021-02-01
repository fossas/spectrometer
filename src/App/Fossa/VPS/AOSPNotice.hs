module App.Fossa.VPS.AOSPNotice
  ( aospNoticeMain,
    WriteEnabled(..)
  ) where

import Control.Effect.Lift (Lift)
import Control.Carrier.Diagnostics
import Effect.Exec
import System.Exit (exitFailure)

import App.Fossa.EmbeddedBinary
import App.Fossa.VPS.Scan.RunWiggins
import App.Fossa.VPS.Types
import App.Types (BaseDir (..))
import Data.Flag (Flag, fromFlag)
import Effect.Logger ( Severity, logDebug, withLogger )

-- | WriteEnabled bool flag
data WriteEnabled = WriteEnabled

aospNoticeMain :: BaseDir -> Severity -> FilterExpressions -> Flag WriteEnabled ->  IO ()
aospNoticeMain basedir logSeverity fileFilters writeEnabled = do
  result <- runDiagnostics $ withWigginsBinary $ aospNoticeGenerate basedir logSeverity writeEnabled fileFilters
  case result of
    Left failure -> do
      print $ renderFailureBundle failure
      exitFailure
    Right _ -> pure ()

----- main logic

aospNoticeGenerate ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) => BaseDir -> Severity -> Flag WriteEnabled -> FilterExpressions -> BinaryPaths -> m ()
aospNoticeGenerate (BaseDir basedir) logSeverity writeEnabled fileFilters binaryPaths = withLogger logSeverity $ do
  let wigginsOpts = generateWigginsAOSPNoticeOpts basedir logSeverity fileFilters (fromFlag WriteEnabled writeEnabled)

  logDebug "Running VPS plugin: generating AOSP notice files"
  runExecIO $ runWiggins binaryPaths wigginsOpts

runWiggins :: ( Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m ()
runWiggins binaryPaths opts = do
  execWiggins binaryPaths opts