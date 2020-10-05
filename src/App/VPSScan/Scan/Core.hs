{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module App.VPSScan.Scan.Core
  ( coreAuthHeader
  , createCoreProject
  , completeCoreProject
  , getSherlockInfo
  , createLocator
  , createRevisionLocator
  , buildRevision
  , overrideScanFilters
  , storeUpdatedScanFilters
  , toFilterExpressions
  , Locator(..)
  , RevisionLocator(..)
  , SherlockInfo(..)
  )
where

import App.VPSScan.Types
import App.Util (parseUri)
import Data.Text (unpack, pack, Text)
import Prelude
import Network.HTTP.Req
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Control.Carrier.Trace.Printing
import Control.Effect.Diagnostics
import Control.Effect.Lift (Lift, sendIO)
import Data.Aeson
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.ByteString.Lazy as BSL

data SherlockInfo = SherlockInfo
  { sherlockUrl :: Text
  , sherlockClientToken :: Text
  , sherlockClientId :: Text
  , sherlockOrgId :: Int
  }
  deriving (Eq, Ord, Show)

instance FromJSON SherlockInfo where
  parseJSON = withObject "SherlockInfo" $ \obj -> do
    auth <- obj .: "auth"
    SherlockInfo <$> obj .: "url" <*> auth .: "clientToken" <*> auth .: "clientId" <*> obj .: "orgId"

data CoreFilterExpressions = CoreFilterExpressions
  { filters :: [Text]
  }
  deriving (Eq, Ord, Show)

instance FromJSON CoreFilterExpressions where
  parseJSON = withObject "CoreFilterExpressions" $ \obj -> CoreFilterExpressions <$> obj .: "filters"

instance ToJSON CoreFilterExpressions where
  toJSON (CoreFilterExpressions filters) = object ["filters" .= filters]

toFilterExpressions :: CoreFilterExpressions -> FilterExpressions
toFilterExpressions coreFilterExpressions = do
  let encoded = decodeUtf8 $ BSL.toStrict $ encode (filters coreFilterExpressions)
  FilterExpressions encoded

toCoreFilterExpressions :: FilterExpressions -> Either String CoreFilterExpressions
toCoreFilterExpressions filterBlob = do
  let blob = "{\"filters\":" <> (unFilterExpressions filterBlob) <> "}"
  eitherDecode $ BSL.fromStrict $ (encodeUtf8 blob)


coreAuthHeader :: Text -> Option scheme
coreAuthHeader apiKey = header "Authorization" (encodeUtf8 ("Bearer " <> apiKey))

buildRevision :: Has (Lift IO) sig m => Maybe Text -> m Text
buildRevision (Just userProvidedRevision) = pure (userProvidedRevision)
buildRevision Nothing = do
  posixTime <- sendIO getPOSIXTime
  pure (pack $ show $ (floor $ toRational posixTime :: Int))

newtype Locator = Locator { unLocator :: Text }

createLocator :: Text -> Int -> Locator
createLocator projectName organizationId = Locator $ "custom+" <> (pack $ show organizationId) <> "/" <> projectName

newtype RevisionLocator = RevisionLocator { unRevisionLocator :: Text }

createRevisionLocator :: Text -> Int -> Text -> RevisionLocator
createRevisionLocator projectName organizationId revision = do
  let locator = createLocator projectName organizationId
  RevisionLocator $ unLocator locator <> "$" <> revision

-- /api/vendored-package-scan/sherlock-info
sherlockInfoEndpoint :: Url 'Https -> Url 'Https
sherlockInfoEndpoint baseurl = baseurl /: "api" /: "vendored-package-scan" /: "sherlock-info"

-- /api/vendored-package-scan/ci
createProjectEndpoint :: Url 'Https -> Url 'Https
createProjectEndpoint baseurl = baseurl /: "api" /: "vendored-package-scan" /: "ci"

-- /api/vendored-package-scan/ci/complete
completeProjectEndpoint :: Url 'Https -> Url 'Https
completeProjectEndpoint baseurl = baseurl /: "api" /: "vendored-package-scan" /: "ci" /: "complete"

-- /api/vendored-package-scan/project-scan-filters/:locator
projectScanFiltersEndpoint :: Url 'Https -> Locator -> Url 'Https
projectScanFiltersEndpoint baseurl locator = baseurl /: "api" /: "vendored-package-scan" /: "project-scan-filters" /: (unLocator locator)

createCoreProject :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Text -> Text -> FossaOpts -> m ()
createCoreProject name revision FossaOpts{..} = runHTTP $ do
  let auth = coreAuthHeader fossaApiKey
  let body = object ["name" .= name, "revision" .= revision]

  (baseUrl, baseOptions) <- parseUri fossaUrl
  _ <- req POST (createProjectEndpoint baseUrl) (ReqBodyJson body) ignoreResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure ()

completeCoreProject :: (Has (Lift IO) sig m, Has Diagnostics sig m) => RevisionLocator -> FossaOpts -> m ()
completeCoreProject locator FossaOpts{..} = runHTTP $ do
  let auth = coreAuthHeader fossaApiKey
  let body = object ["locator" .= (unRevisionLocator locator)]

  (baseUrl, baseOptions) <- parseUri fossaUrl
  _ <- req POST (completeProjectEndpoint baseUrl) (ReqBodyJson body) ignoreResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure ()

getSherlockInfo :: (Has (Lift IO) sig m, Has Diagnostics sig m) => FossaOpts -> m SherlockInfo
getSherlockInfo FossaOpts{..} = runHTTP $ do
  let auth = coreAuthHeader fossaApiKey

  (baseUrl, baseOptions) <- parseUri fossaUrl
  resp <- req GET (sherlockInfoEndpoint baseUrl) NoReqBody jsonResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure (responseBody resp)

getProjectScanFilters :: (Has (Lift IO) sig m, Has Diagnostics sig m) => FossaOpts -> Locator -> m CoreFilterExpressions
getProjectScanFilters FossaOpts{..} locator = runHTTP $ do
  let auth = coreAuthHeader fossaApiKey

  (baseUrl, baseOptions) <- parseUri fossaUrl
  resp <- req GET (projectScanFiltersEndpoint baseUrl locator) NoReqBody jsonResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
  pure (responseBody resp)

overrideScanFilters :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has Trace sig m) => VPSOpts -> Locator -> m (VPSOpts, Bool)
overrideScanFilters vpsOpts@VPSOpts{..} locator
  | (unFilterExpressions filterBlob) == "[]" = do
    trace "[All] Fetching scan file filter from FOSSA"
    overrideFilters <- getProjectScanFilters fossa locator
    let overrideFilterBlob = toFilterExpressions overrideFilters
    trace $ unpack $ "[All] Using filter: " <> (unFilterExpressions overrideFilterBlob)
    pure (VPSOpts fossa projectName userProvidedRevision skipIprScan overrideFilterBlob, True)
  | otherwise = do
    trace "[All] Scan file filters provided locally"
    pure (vpsOpts, False)

storeUpdatedScanFilters :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has Trace sig m) => Locator -> FilterExpressions -> FossaOpts -> m ()
storeUpdatedScanFilters locator filterBlob FossaOpts{..}
  | (unFilterExpressions filterBlob) == "[]" = do
    trace "[All] No scan file filter was set, skipping update"
    pure ()
  | otherwise = runHTTP $ do
    let auth = coreAuthHeader fossaApiKey
    case toCoreFilterExpressions filterBlob of
      Right body -> do
        trace "[All] Updating FOSSA with new scan file filter for this project"
        (baseUrl, baseOptions) <- parseUri fossaUrl
        _ <- req POST (projectScanFiltersEndpoint baseUrl locator) (ReqBodyJson body) ignoreResponse (baseOptions <> header "Content-Type" "application/json" <> auth)
        pure ()
      Left err -> do
        -- In reality, if this were to happen, `ramjet-cli-ipr` and `sherlock-cli` would have failed before we got to this point
        -- because they require the filter expressions to be valid JSON format.
        trace $ "[All] Failed to encode filter to JSON: " <> err
        pure ()
