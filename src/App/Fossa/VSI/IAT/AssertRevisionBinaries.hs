module App.Fossa.VSI.IAT.AssertRevisionBinaries (
  assertRevisionBinaries,
) where

import App.Fossa.FossaAPIV1 qualified as Fossa
import App.Fossa.VSI.IAT.Fingerprint
import Control.Algebra
import Control.Carrier.Diagnostics
import Control.Effect.Lift
import Effect.Logger
import Effect.ReadFS
import Fossa.API.Types
import Path
import Srclib.Types (Locator)

assertRevisionBinaries :: (Has Diagnostics sig m, Has ReadFS sig m, Has (Lift IO) sig m, Has Logger sig m) => Path Abs Dir -> ApiOpts -> Locator -> m ()
assertRevisionBinaries dir apiOpts locator = do
  logInfo "Fingerprinting assertion directory contents"
  fingerprints <- fingerprintContentsRaw dir

  logInfo "Uploading assertion to FOSSA"
  Fossa.assertRevisionBinaries apiOpts locator fingerprints

  pure ()
