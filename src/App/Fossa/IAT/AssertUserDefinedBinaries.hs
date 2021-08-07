module App.Fossa.IAT.AssertUserDefinedBinaries (
  assertUserDefinedBinariesMain,
) where

import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.IAT.Fingerprint
import App.Fossa.IAT.Types
import App.Types
import Control.Algebra
import Control.Carrier.Diagnostics
import Control.Effect.Lift
import Effect.Logger
import Effect.ReadFS
import Fossa.API.Types
import Path

assertUserDefinedBinariesMain :: Severity -> BaseDir -> ApiOpts -> UserDefinedAssertionMeta -> IO ()
assertUserDefinedBinariesMain logSeverity (BaseDir dir) apiOpts assertion = withDefaultLogger logSeverity . logWithExit_ . runReadFSIO $ do
  assertUserDefinedBinaries dir apiOpts assertion

assertUserDefinedBinaries :: (Has Diagnostics sig m, Has ReadFS sig m, Has (Lift IO) sig m, Has Logger sig m) => Path Abs Dir -> ApiOpts -> UserDefinedAssertionMeta -> m ()
assertUserDefinedBinaries dir apiOpts assertionMeta = do
  logInfo "Fingerprinting directory contents"
  fingerprints <- fingerprintContentsRaw dir

  logInfo "Uploading assertion to FOSSA"
  Fossa.assertUserDefinedBinaries apiOpts assertionMeta fingerprints

  pure ()
