{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module App.VPSScan.Scan.ScotlandYard
  ( createScotlandYardScan
  , uploadIPRResults
  , uploadBuildGraph
  , ScanResponse (..)
  , ScotlandYardOpts (..)
  , ScotlandYardNinjaOpts (..)
  )
where

import App.VPSScan.Types
import App.VPSScan.Scan.Core
import Control.Effect.Diagnostics
import Control.Effect.Lift (Lift)
import Data.Aeson
import Data.Text (Text)
import Network.HTTP.Req
import App.Util (parseUri)

data ScotlandYardOpts = ScotlandYardOpts
  { projectId :: Locator
  , projectRevision :: Text
  , organizationId :: Int
  , syVpsOpts :: VPSOpts
  }

data ScotlandYardNinjaOpts = ScotlandYardNinjaOpts
  { syNinjaProjectId :: Locator
  , syNinjaOrganizationId :: Int
  , syNinjaOpts :: NinjaGraphOpts
  }

-- Prefix for Core's reverse proxy to SY
coreProxyPrefix :: Url 'Https -> Url 'Https
coreProxyPrefix baseurl = baseurl /: "api" /: "proxy" /: "scotland-yard"

-- /projects/{projectID}/scans
createScanEndpoint :: Url 'Https -> Text -> Url 'Https
createScanEndpoint baseurl projectId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans"

-- /projects/{projectID}/scans/{scanID}/discovered_licenses
scanDataEndpoint :: Url 'Https -> Text -> Text -> Url 'Https
scanDataEndpoint baseurl projectId scanId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: scanId /: "discovered_licenses"

data ScanResponse = ScanResponse
  { responseScanId :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON ScanResponse where
  parseJSON = withObject "ScanResponse" $ \obj ->
    ScanResponse <$> obj .: "scanId"

data CreateBuildGraphResponse = CreateBuildGraphResponse
  { responseBuildGraphId :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON CreateBuildGraphResponse where
  parseJSON = withObject "CreateBuildGraphResponse" $ \obj ->
    CreateBuildGraphResponse <$> obj .: "ID"

createScotlandYardScan :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ScotlandYardOpts -> m ScanResponse
createScotlandYardScan ScotlandYardOpts {..} = runHTTP $ do
  let VPSOpts{..} = syVpsOpts
  let FossaOpts{..} = fossa

  let body = object ["revisionId" .= projectRevision, "organizationId" .= organizationId]
  let auth = coreAuthHeader fossaApiKey
  let locator = unLocator projectId

  (baseUrl, baseOptions) <- parseUri fossaUrl
  resp <- req POST (createScanEndpoint baseUrl locator) (ReqBodyJson body) jsonResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure (responseBody resp)

-- Given the results from a run of IPR, a scan ID and a URL for Scotland Yard,
-- post the IPR result to the "Upload Scan Data" endpoint on Scotland Yard
-- POST /scans/{scanID}/discovered_licenses
uploadIPRResults :: (ToJSON a, Has (Lift IO) sig m, Has Diagnostics sig m) => Text -> a -> ScotlandYardOpts -> m ()
uploadIPRResults scanId value ScotlandYardOpts {..} = runHTTP $ do
  let VPSOpts{..} = syVpsOpts
  let FossaOpts{..} = fossa
  let auth = coreAuthHeader fossaApiKey
  let locator = unLocator projectId

  (baseUrl, baseOptions) <- parseUri fossaUrl
  _ <- req POST (scanDataEndpoint baseUrl locator scanId) (ReqBodyJson value) ignoreResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure ()

-- /projects/{projectID}/scans/{scanID}/build-graphs
createBuildGraphEndpoint :: Url 'Https -> Text -> Text -> Url 'Https
createBuildGraphEndpoint baseurl projectId scanId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: scanId /: "build-graphs"

-- /projects/{projectID}/scans/{scanID}/build-graphs/{buildGraphID}/rules
uploadBuildGraphChunkEndpoint :: Url 'Https -> Text -> Text -> Text ->  Url 'Https
uploadBuildGraphChunkEndpoint baseurl projectId scanId buildGraphId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: scanId /: "build-graphs" /: buildGraphId /: "rules"

-- /projects/{projectID}/scans/{scanID}/build-graphs/{buildGraphID}/rules/complete
uploadBuildGraphCompleteEndpoint :: Url 'Https -> Text -> Text -> Text ->  Url 'Https
uploadBuildGraphCompleteEndpoint baseurl projectId scanId buildGraphId = coreProxyPrefix baseurl /: "projects" /: projectId /: "scans" /: scanId /: "build-graphs" /: buildGraphId /: "rules" /: "complete"

-- create the build graph in SY, upload it in chunks and then complete it.
uploadBuildGraph :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ScotlandYardNinjaOpts -> [DepsTarget] -> m ()
uploadBuildGraph ScotlandYardNinjaOpts {..} graph = runHTTP $ do
  let NinjaGraphOpts{..} = syNinjaOpts
  let FossaOpts{..} = ninjaFossaOpts
  let auth = coreAuthHeader fossaApiKey
  let locator = unLocator syNinjaProjectId
  let body = object ["display_name" .= buildName]
  (baseUrl, baseOptions) <- parseUri fossaUrl
  resp <- req POST (createBuildGraphEndpoint baseUrl locator scanId) (ReqBodyJson body) jsonResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  let buildGraphId = CreateBuildGraphResponse $ responseBody resp
  _ <- req POST (uploadBuildGraphChunkEndpoint baseUrl locator scanId (responseBuildGraphId buildGraphId)) (ReqBodyJson graph) ignoreResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  _ <- req POST (uploadBuildGraphCompleteEndpoint baseUrl locator scanId (responseBuildGraphId buildGraphId)) (ReqBodyJson $ object []) ignoreResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure ()
