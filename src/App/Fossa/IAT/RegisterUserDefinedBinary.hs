{-# LANGUAGE RecordWildCards #-}

module App.Fossa.IAT.RegisterUserDefinedBinary (
  registerUserDefinedIATBinaryMain,
) where

import App.Fossa.FossaAPIV1
import App.Fossa.IAT.Fingerprint
import App.Fossa.IAT.Types
import Control.Algebra
import Control.Carrier.Diagnostics
import Control.Effect.Lift
import Effect.Logger
import Effect.ReadFS
import Fossa.API.Types
import Path

registerUserDefinedIATBinaryMain :: Severity -> Path Abs Dir -> ApiOpts -> UserDefinedBinaryAssertion -> IO ()
registerUserDefinedIATBinaryMain logSeverity dir apiOpts assertion = withDefaultLogger logSeverity . logWithExit_ . runReadFSIO $ do
  registerUserDefinedIATBinary dir apiOpts assertion

registerUserDefinedIATBinary :: (Has Diagnostics sig m, Has ReadFS sig m, Has (Lift IO) sig m, Has Logger sig m) => Path Abs Dir -> ApiOpts -> UserDefinedBinaryAssertion -> m ()
registerUserDefinedIATBinary dir apiOpts UserDefinedBinaryAssertion{..} = do
  logInfo "Fingerprinting directory contents"
  fingerprints <- fingerprintContentsRaw dir

  logInfo "Uploading assertion to FOSSA"
  let assertion = UserDefinedIATBinaryAssertion assertedName assertedVersion assertedLicenseIdentifier assertedDescription assertedUrl (IATFingerprintSet <$> fingerprints)
  assertIATUserDefinedBinaries apiOpts assertion

  pure ()