{-# LANGUAGE RecordWildCards #-}

module App.Fossa.IAT.RegisterUserDefinedBinary (
  registerUserDefinedBinaryMain,
) where

import App.Fossa.FossaAPIV1
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

registerUserDefinedBinaryMain :: Severity -> BaseDir -> ApiOpts -> UserDefinedBinaryAssertion -> IO ()
registerUserDefinedBinaryMain logSeverity (BaseDir dir) apiOpts assertion = withDefaultLogger logSeverity . logWithExit_ . runReadFSIO $ do
  registerUserDefinedBinary dir apiOpts assertion

registerUserDefinedBinary :: (Has Diagnostics sig m, Has ReadFS sig m, Has (Lift IO) sig m, Has Logger sig m) => Path Abs Dir -> ApiOpts -> UserDefinedBinaryAssertion -> m ()
registerUserDefinedBinary dir apiOpts UserDefinedBinaryAssertion{..} = do
  logInfo "Fingerprinting directory contents"
  fingerprints <- fingerprintContentsRaw dir

  logInfo "Uploading assertion to FOSSA"
  let assertion = UserDefinedAssertion assertedName assertedVersion assertedLicenseIdentifier assertedDescription assertedUrl fingerprints
  assertUserDefinedBinaries apiOpts assertion

  pure ()
