module App.Scan.FossaV1
  ( uploadAnalysis
  , UploadResponse(..)
  , FossaError(..)
  , fossaReq

  , getLatestBuild
  , Build(..)
  , BuildTask(..)
  , BuildStatus(..)
  , getIssues
  , Issues(..)
  , Issue(..)
  , IssueType(..)
  , renderIssueType
  , IssueRule(..)

  , getOrganizationId
  ) where

import App.Scan.Project
import Control.Carrier.Error.Either
import Data.List (isInfixOf)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Effect.Logger
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Req
import qualified Network.HTTP.Types as HTTP
import Prologue
import Srclib.Converter (toSourceUnit)
import Srclib.Types

-- TODO: git commit?
cliVersion :: Text
cliVersion = "spectrometer"

uploadUrl :: Url 'Https
uploadUrl = https "app.fossa.com" /: "api" /: "builds" /: "custom"
 
-- FIXME: we only want to include organizationId for "archive" and "custom"
-- TODO: need to normalize "git" projects
-- render a locator for use in fossa API urls
renderLocatorUrl :: Int -> Locator -> Text
renderLocatorUrl orgId Locator{..} =
  locatorFetcher <> "+" <> T.pack (show orgId) <> "/" <> locatorProject <> "$" <> fromMaybe "" locatorRevision

newtype FossaReq m a = FossaReq { runFossaReq :: ErrorC FossaError m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, Effect sig, MonadIO m) => MonadHttp (FossaReq m) where
  handleHttpException = FossaReq . throwError . mangleError

data UploadResponse = UploadResponse
  { uploadLocator :: Text
  , uploadError   :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON UploadResponse where
  parseJSON = withObject "UploadResponse" $ \obj ->
    UploadResponse <$> obj .: "locator"
                   <*> obj .:? "error"

data FossaError
  = InvalidProjectOrRevision HttpException
  | NoPermission HttpException
  | JsonDeserializeError String
  | OtherError HttpException
  deriving (Show, Generic)

fossaReq :: FossaReq m a -> m (Either FossaError a)
fossaReq = runError . runFossaReq

uploadAnalysis
  :: Text -- api key
  -> Text -- project name
  -> Text -- project revision
  -> [Project]
  -> IO (Either FossaError UploadResponse)
uploadAnalysis key name revision projects = fossaReq $ do
  let filteredProjects = filter (isProductionPath . projectPath) projects
      sourceUnits = fromMaybe [] $ traverse toSourceUnit filteredProjects
      opts = "locator" =: renderLocator (Locator "custom" name (Just revision))
          <> "v" =: cliVersion
          <> "managedBuild" =: True
          <> "title" =: name
          <> header "Authorization" ("token " <> encodeUtf8 key)
  resp <- req POST uploadUrl (ReqBodyJson sourceUnits) jsonResponse opts
  pure (responseBody resp)

mangleError :: HttpException -> FossaError
mangleError err = case err of
  VanillaHttpException (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException resp _)) ->
    case HTTP.responseStatus resp of
      HTTP.Status 404 _ -> InvalidProjectOrRevision err
      HTTP.Status 403 _ -> NoPermission err
      _                 -> OtherError err
  JsonHttpException msg -> JsonDeserializeError msg
  _ -> OtherError err

-- we specifically want Rel paths here: parent directories shouldn't affect path
-- filtering
isProductionPath :: Path Rel fd -> Bool
isProductionPath path = not $ any (`isInfixOf` toFilePath path)
  [ "doc/"
  , "docs/"
  , "test/"
  , "example/"
  , "examples/"
  , "vendor/"
  , "node_modules/"
  , ".srclib-cache/"
  , "spec/"
  , "Godeps/"
  , ".git/"
  , "bower_components/"
  , "third_party/"
  , "third-party/"
  , "tmp/"
  , "Carthage/"
  , "Checkouts/"
  ]

-----

buildsEndpoint :: Int -> Locator -> Url 'Https
buildsEndpoint orgId locator = https "app.fossa.com" /: "api" /: "cli" /: renderLocatorUrl orgId locator /: "latest_build"

data BuildStatus
  = StatusSucceeded
  | StatusFailed
  | StatusCreated
  | StatusAssigned
  | StatusRunning
  | StatusUnknown Text
  deriving (Eq, Ord, Show, Generic)

data Build = Build
  { buildId :: Int
  , buildError :: Maybe Text
  , buildTask :: BuildTask
  } deriving (Eq, Ord, Show, Generic)

data BuildTask = BuildTask
  { buildTaskStatus :: BuildStatus
  } deriving (Eq, Ord, Show, Generic)

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

getLatestBuild
  :: Text -- ^ api key
  -> Text -- ^ project name
  -> Text -- ^ project revision
  -> IO (Either FossaError Build)
getLatestBuild key name revision = fossaReq $ do
  let opts = header "Authorization" ("token " <> encodeUtf8 key)
  Organization orgId <- responseBody <$> req GET organizationEndpoint NoReqBody jsonResponse opts
  response <- req GET (buildsEndpoint orgId (Locator "custom" name (Just revision))) NoReqBody jsonResponse opts
  pure (responseBody response)

----------

issuesEndpoint :: Int -> Locator -> Url 'Https
issuesEndpoint orgId locator = https "app.fossa.com" /: "api" /: "cli" /: renderLocatorUrl orgId locator /: "issues"

getIssues
  :: Text -- ^ api key
  -> Text -- ^ project name
  -> Text -- ^ project revision
  -> IO (Either FossaError Issues)
getIssues key name revision = fossaReq $ do
  let opts = header "Authorization" ("token " <> encodeUtf8 key)
  Organization orgId <- responseBody <$> req GET organizationEndpoint NoReqBody jsonResponse opts
  response <- req GET (issuesEndpoint orgId (Locator "custom" name (Just revision))) NoReqBody jsonResponse opts
  pure (responseBody response)

data Issues = Issues
  { issuesCount :: Int
  , issuesIssues :: [Issue]
  , issuesStatus :: Text
  } deriving (Eq, Ord, Show, Generic)

data IssueType
  = IssuePolicyConflict
  | IssuePolicyFlag
  | IssueVulnerability
  | IssueUnlicensedDependency
  | IssueOutdatedDependency
  | IssueOther Text
  deriving (Eq, Ord, Show, Generic)

renderIssueType :: IssueType -> Text
renderIssueType = \case
  IssuePolicyConflict -> "Denied by Policy"
  IssuePolicyFlag -> "Denied by Policy"
  IssueVulnerability -> "Denied by Policy"
  IssueUnlicensedDependency -> "Denied by Policy"
  IssueOutdatedDependency -> "Denied by Policy"
  IssueOther other -> other

data Issue = Issue
  { issueId :: Int
  , issueResolved :: Bool
  , issueRevisionId :: Text
  , issueType :: IssueType
  , issueRule :: Maybe IssueRule
  } deriving (Eq, Ord, Show, Generic)

data IssueRule = IssueRule
  { ruleLicenseId :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Issues where
  parseJSON = withObject "Issues" $ \obj ->
    Issues <$> obj .: "count"
           <*> obj .: "issues"
           <*> obj .: "status"

instance FromJSON Issue where
  parseJSON = withObject "Issue" $ \obj ->
    Issue <$> obj .: "id"
           <*> obj .: "resolved"
           <*> obj .: "revisionId"
           <*> obj .: "type"
           <*> obj .:? "rule"

instance FromJSON IssueType where
  parseJSON = withText "IssueType" $ \case
    "policy_conflict" -> pure IssuePolicyConflict
    "policy_flag" -> pure IssuePolicyFlag
    "vulnerability" -> pure IssueVulnerability
    "unlicensed_dependency" -> pure IssueUnlicensedDependency
    "outdated_dependency" -> pure IssueOutdatedDependency
    other -> pure (IssueOther other)

instance FromJSON IssueRule where
  parseJSON = withObject "IssueRule" $ \obj ->
    IssueRule <$> obj .:? "licenseId"

----------

data Organization = Organization
  { organizationId :: Int
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Organization where
  parseJSON = withObject "Organization" $ \obj ->
    Organization <$> obj .: "organizationId"

organizationEndpoint :: Url 'Https
organizationEndpoint = https "app.fossa.com" /: "api" /: "cli" /: "organization"

getOrganizationId
  :: Text -- ^ api key
  -> IO (Either FossaError Issues)
getOrganizationId key = fossaReq $ do
  let opts = header "Authorization" ("token " <> encodeUtf8 key)
  response <- req GET organizationEndpoint NoReqBody jsonResponse opts
  pure (responseBody response)
