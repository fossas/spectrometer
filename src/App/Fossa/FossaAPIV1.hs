{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Fossa.FossaAPIV1
  ( uploadAnalysis,
    uploadContributors,
    uploadContainerScan,
    UploadResponse (..),
    mkMetadataOpts,
    FossaError (..),
    FossaReq (..),
    Contributors (..),
    fossaReq,
    getLatestBuild,
    Build (..),
    BuildTask (..),
    BuildStatus (..),
    getIssues,
    Organization (..),
    getOrganization,
    getAttribution,
    getAttributionRaw,
    getSignedURL,
    archiveUpload,
    archiveBuildUpload,
    ArchiveComponents (..),
    Archive (..),
  )
where

import Prelude
import App.Fossa.Analyze.Project
import App.Fossa.Container (ContainerScan (..))
import App.Fossa.Report.Attribution qualified as Attr
import App.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import App.Version (versionNumber)
import Control.Effect.Diagnostics hiding (fromMaybe)
import Control.Effect.Lift (Lift, sendIO)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Effect.Logger
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Req
import Network.HTTP.Types qualified as HTTP
import Srclib.Types
import Text.URI (URI)
import Text.URI qualified as URI
import Fossa.API.Types (ApiOpts, Issues, SignedURL, useApiOpts, signedURL)
import Network.HTTP.Req
import Network.HTTP.Client qualified as C
import Data.Word (Word8)

newtype FossaReq m a = FossaReq {unFossaReq :: m a}
  deriving (Functor, Applicative, Monad, Algebra sig)

instance Has (Lift IO) sig m => MonadIO (FossaReq m) where
  liftIO = sendIO

instance (Has (Lift IO) sig m, Has Diagnostics sig m) => MonadHttp (FossaReq m) where
  handleHttpException = FossaReq . fatal . mangleError

fossaReq :: FossaReq m a -> m a
fossaReq = unFossaReq

-- Don't send any version if one doesn't exist
cliVersion :: Text
cliVersion = fromMaybe "" versionNumber

uploadUrl :: Url scheme -> Url scheme
uploadUrl baseurl = baseurl /: "api" /: "builds" /: "custom"

-- | This renders an organization + locator into a path piece for the fossa API
renderLocatorUrl :: Int -> Locator -> Text
renderLocatorUrl orgId Locator{..} =
  locatorFetcher <> "+" <> T.pack (show orgId) <> "/" <> normalizeGitProjectName locatorProject <> "$" <> fromMaybe "" locatorRevision

-- | The fossa backend treats http git locators in a specific way for the issues and builds endpoints.
-- This normalizes a project name to conform to what the API expects
normalizeGitProjectName :: Text -> Text
normalizeGitProjectName project
  | "http" `T.isPrefixOf` project = dropPrefix "http://" . dropPrefix "https://" . dropSuffix ".git" $ project
  | otherwise = project
  where
    -- like Text.stripPrefix, but with a non-Maybe result (defaults to the original text)
    dropPrefix :: Text -> Text -> Text
    dropPrefix pre txt = fromMaybe txt (T.stripPrefix pre txt)

    -- like Text.stripSuffix, but with a non-Maybe result (defaults to the original text)
    dropSuffix :: Text -> Text -> Text
    dropSuffix suf txt = fromMaybe txt (T.stripSuffix suf txt)

data UploadResponse = UploadResponse
  { uploadLocator :: Text
  , uploadError :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON UploadResponse where
  parseJSON = withObject "UploadResponse" $ \obj ->
    UploadResponse <$> obj .: "locator"
      <*> obj .:? "error"

data FossaError
  = InvalidProjectOrRevision HttpException
  | NoPermission HttpException
  | JsonDeserializeError String
  | OtherError HttpException
  | BadURI URI
  deriving (Show)

instance ToDiagnostic FossaError where
  renderDiagnostic = \case
    InvalidProjectOrRevision _ -> "Response from FOSSA API: invalid project or revision"
    NoPermission _ -> "Response from FOSSA API: no permission"
    JsonDeserializeError err -> "An error occurred when deserializing a response from the FOSSA API: " <> pretty err
    OtherError err -> "An unknown error occurred when accessing the FOSSA API: " <> viaShow err
    BadURI uri -> "Invalid FOSSA URL: " <> pretty (URI.render uri)

containerUploadUrl :: Url scheme -> Url scheme
containerUploadUrl baseurl = baseurl /: "api" /: "container" /: "upload"

uploadContainerScan ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  ProjectMetadata ->
  ContainerScan ->
  m UploadResponse
uploadContainerScan apiOpts ProjectRevision{..} metadata scan = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  let locator = renderLocator $ Locator "custom" projectName (Just projectRevision)
      opts =
        "locator" =: locator
          <> "cliVersion" =: cliVersion
          <> "managedBuild" =: True
          <> maybe mempty ("branch" =:) projectBranch
          <> mkMetadataOpts metadata projectName
  resp <- req POST (containerUploadUrl baseUrl) (ReqBodyJson scan) jsonResponse (baseOpts <> opts)
  pure $ responseBody resp

uploadAnalysis ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  ProjectMetadata ->
  NE.NonEmpty SourceUnit ->
  m UploadResponse
uploadAnalysis apiOpts ProjectRevision{..} metadata sourceUnits = fossaReq $ do
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

mkMetadataOpts :: ProjectMetadata -> Text -> Option scheme
mkMetadataOpts ProjectMetadata{..} projectName = mconcat $ catMaybes maybes
  where
    title = Just $ fromMaybe projectName projectTitle
    maybes =
      [ ("projectURL" =:) <$> projectUrl
      , ("jiraProjectKey" =:) <$> projectJiraKey
      , ("link" =:) <$> projectLink
      , ("team" =:) <$> projectTeam
      , ("policy" =:) <$> projectPolicy
      , ("title" =:) <$> title
      ]

mangleError :: HttpException -> FossaError
mangleError err = case err of
  VanillaHttpException (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException resp _)) ->
    case HTTP.responseStatus resp of
      HTTP.Status 404 _ -> InvalidProjectOrRevision err
      HTTP.Status 403 _ -> NoPermission err
      _ -> OtherError err
  JsonHttpException msg -> JsonDeserializeError msg
  _ -> OtherError err

-----

buildsEndpoint :: Url 'Https -> Int -> Locator -> Url 'Https
buildsEndpoint baseurl orgId locator = baseurl /: "api" /: "cli" /: renderLocatorUrl orgId locator /: "latest_build"

data BuildStatus
  = StatusSucceeded
  | StatusFailed
  | StatusCreated
  | StatusAssigned
  | StatusRunning
  | StatusUnknown Text
  deriving (Eq, Ord, Show)

data Build = Build
  { buildId :: Int
  , buildError :: Maybe Text
  , buildTask :: BuildTask
  }
  deriving (Eq, Ord, Show)

newtype BuildTask = BuildTask
  { buildTaskStatus :: BuildStatus
  }
  deriving (Eq, Ord, Show)

instance FromJSON Build where
  parseJSON = withObject "Build" $ \obj ->
    Build <$> obj .: "id"
      <*> obj .:? "error"
      <*> obj .: "task"

instance FromJSON BuildTask where
  parseJSON = withObject "BuildTask" $ \obj ->
    BuildTask <$> obj .: "status"

instance FromJSON BuildStatus where
  parseJSON = withText "BuildStatus" $ \case
    "SUCCEEDED" -> pure StatusSucceeded
    "FAILED" -> pure StatusFailed
    "CREATED" -> pure StatusCreated
    "ASSIGNED" -> pure StatusAssigned
    "RUNNING" -> pure StatusRunning
    other -> pure $ StatusUnknown other

getLatestBuild ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m Build
getLatestBuild apiOpts ProjectRevision{..} = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  Organization orgId _ <- getOrganization apiOpts

  response <- req GET (buildsEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse baseOpts
  pure (responseBody response)

---------- Archive build queueing. This Endpoint ensures that after an archive is uploaded, it is scanned.

newtype ArchiveComponents = ArchiveComponents
  { archives :: [Archive]
  }

data Archive = Archive
 { packageSpec :: Text
 , revision :: Text
 }

instance ToJSON ArchiveComponents where
 toJSON ArchiveComponents{..} = object
   [ "archives" .= archives
   ]

instance ToJSON Archive where
 toJSON Archive{..} = object
   [ "packageSpec" .= packageSpec
   , "revision" .= revision
   ]

archiveBuildURL :: Url 'Https -> Url 'Https
archiveBuildURL baseUrl = baseUrl /: "api" /: "components" /: "build"

archiveBuildUpload ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ArchiveComponents ->
  m C.ByteString
archiveBuildUpload apiOpts archiveProjects = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts = "dependency" =: True
           <> "rawLicenseScan" =: True

  resp <- req POST (archiveBuildURL baseUrl) (ReqBodyJson archiveProjects) bsResponse (baseOpts <> opts)
  pure (responseBody resp)

---------- The signed URL endpoint returns a URL endpoint that can be used to directly upload to an S3 bucket.

signedURLEndpoint :: Url 'Https -> Url 'Https
signedURLEndpoint baseUrl = baseUrl /: "api" /: "components" /: "signed_url"

getSignedURL ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  Text ->
  Text ->
  m SignedURL
getSignedURL apiOpts revision packageName = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts = "packageSpec" =: packageName <> "revision" =: revision

  response <- req GET (signedURLEndpoint baseUrl) NoReqBody jsonResponse (baseOpts <> opts)
  pure (responseBody response)

---------- The archive upload function uploads the file it is given directly to the signed URL it is provided.

archiveUpload ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  SignedURL ->
  FilePath ->
  m String
archiveUpload signedArcURI arcFile = fossaReq $ do
        let arcURL = mkURI $ signedURL signedArcURI

        case arcURL >>= useHttpsURI of
              Nothing -> case arcURL of
                Nothing -> fatalText ("Error attempting to archive upload file: " <> T.pack arcFile <> ". No signed URL supplied")
                Just resultURL -> fatalText ("Invalid URL: " <> render resultURL)
              Just (url, options) -> do
                    res <- reqCb PUT url (ReqBodyFile arcFile) lbsResponse options (pure . requestEncoder)
                    pure $ show res
                    where

-- requestEncoder properly encodes the Request path. 
-- The default encoding logic does not encode "+" ot "$" characters which makes AWS very angry. 
-- This is accomplished by passing "True" to "Http.urlEncode" to signify that we want to encode more characters.
requestEncoder :: C.Request -> C.Request
requestEncoder r = r { C.path = encoder (C.path r) }

encoder :: BS.ByteString -> BS.ByteString
encoder path = BS.singleton slashWord8 <> joined
  where
    split :: [BS.ByteString]
    split = BS.split slashWord8 path
    filtered :: [BS.ByteString]
    filtered = filter (/= BS.empty) split
    encoded :: [BS.ByteString]
    encoded = map (HTTP.urlEncode True) filtered
    joined :: BS.ByteString
    joined = BS.intercalate (BS.singleton slashWord8) encoded

slashWord8 :: Word8
slashWord8 = fromIntegral $ fromEnum '/'

----------

issuesEndpoint :: Url 'Https -> Int -> Locator -> Url 'Https
issuesEndpoint baseUrl orgId locator = baseUrl /: "api" /: "cli" /: renderLocatorUrl orgId locator /: "issues"

getIssues ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m Issues
getIssues apiOpts ProjectRevision{..} = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  Organization orgId _ <- getOrganization apiOpts
  response <- req GET (issuesEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse baseOpts
  pure (responseBody response)

---------------

attributionEndpoint :: Url 'Https -> Int -> Locator -> Url 'Https
attributionEndpoint baseurl orgId locator = baseurl /: "api" /: "revisions" /: renderLocatorUrl orgId locator /: "attribution" /: "json"

getAttribution ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m Attr.Attribution
getAttribution apiOpts ProjectRevision{..} = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts =
        baseOpts
          <> "includeDeepDependencies" =: True
          <> "includeHashAndVersionData" =: True
          <> "includeDownloadUrl" =: True
  Organization orgId _ <- getOrganization apiOpts
  response <- req GET (attributionEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse opts
  pure (responseBody response)

getAttributionRaw ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m Value
getAttributionRaw apiOpts ProjectRevision{..} = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts =
        baseOpts
          <> "includeDeepDependencies" =: True
          <> "includeHashAndVersionData" =: True
          <> "includeDownloadUrl" =: True
  Organization orgId _ <- getOrganization apiOpts
  response <- req GET (attributionEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse opts
  pure (responseBody response)

----------

data Organization = Organization
  { organizationId :: Int
  , orgUsesSAML :: Bool
  }
  deriving (Eq, Ord, Show)

instance FromJSON Organization where
  parseJSON = withObject "Organization" $ \obj ->
    Organization <$> obj .: "organizationId"
      <*> obj .:? "usesSAML" .!= False

organizationEndpoint :: Url scheme -> Url scheme
organizationEndpoint baseurl = baseurl /: "api" /: "cli" /: "organization"

getOrganization :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> m Organization
getOrganization apiOpts = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  responseBody <$> req GET (organizationEndpoint baseUrl) NoReqBody jsonResponse baseOpts

----------

newtype Contributors = Contributors
  {unContributors :: Map Text Text}
  deriving (Eq, Ord, Show, ToJSON)

contributorsEndpoint :: Url scheme -> Url scheme
contributorsEndpoint baseurl = baseurl /: "api" /: "contributors"

uploadContributors :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> Text -> Contributors -> m ()
uploadContributors apiOpts locator contributors = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts = baseOpts <> "locator" =: locator

  _ <- req POST (contributorsEndpoint baseUrl) (ReqBodyJson contributors) ignoreResponse opts
  pure ()
