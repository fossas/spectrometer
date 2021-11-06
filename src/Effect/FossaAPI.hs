{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Effect.FossaAPI (
  FossaAPIFS (..),
  FossaAPIC,
  FossaAPI,
  uploadAnalysis,
  runFossaAPI,
) where

import App.Fossa.FossaAPIV1 (UploadResponse, cliVersion, fossaReq, mkMetadataOpts, uploadUrl)
import App.Types (ProjectMetadata, ProjectRevision (..))
import Control.Carrier.Simple (Has, Simple, SimpleC, interpret, sendSimple)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Control.Effect.Record.TH (deriveRecordable)
import Data.List.NonEmpty qualified as NE
import Fossa.API.Types (ApiOpts, useApiOpts)
import Network.HTTP.Req (POST (POST), ReqBodyJson (ReqBodyJson), jsonResponse, req, responseBody, (=:))
import Srclib.Types (Locator (Locator), SourceUnit, renderLocator)

data FossaAPIFS a where
  UploadAnalysis :: ApiOpts -> ProjectRevision -> ProjectMetadata -> NE.NonEmpty SourceUnit -> FossaAPIFS (UploadResponse)

type FossaAPI = Simple FossaAPIFS
type FossaAPIC = SimpleC FossaAPIFS

$(deriveRecordable ''FossaAPIFS)

deriving instance Show (FossaAPIFS a)
deriving instance Eq (FossaAPIFS a)
deriving instance Ord (FossaAPIFS a)

uploadAnalysis ::
  (Has (Lift IO) sig m, Has FossaAPI sig m) =>
  ApiOpts ->
  ProjectRevision ->
  ProjectMetadata ->
  NE.NonEmpty SourceUnit ->
  m UploadResponse
uploadAnalysis apiOpts rev meta srcUnit = sendSimple $ UploadAnalysis apiOpts rev meta srcUnit

runFossaAPI :: (Has (Lift IO) sig m, Has Diagnostics sig m) => FossaAPIC m a -> m a
runFossaAPI = interpret $ \case
  UploadAnalysis apiOpts ProjectRevision{..} metadata sourceUnits ->
    fossaReq $ do
      (baseUrl, baseOpts) <- useApiOpts apiOpts
      let opts =
            "locator" =: renderLocator (Locator "custom" projectName (Just projectRevision))
              <> "cliVersion" =: cliVersion
              <> "managedBuild" =: True
              <> mkMetadataOpts metadata projectName
              -- Don't include branch if it doesn't exist, core may not handle empty string properly.
              <> maybe mempty ("branch" =:) projectBranch
      resp <- req POST (uploadUrl baseUrl) (ReqBodyJson $ NE.toList sourceUnits) jsonResponse (baseOpts <> opts)
      pure (responseBody resp)
