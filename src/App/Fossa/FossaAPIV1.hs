module App.Fossa.FossaAPIV1
  ( uploadAnalysis
  , UploadResponse(..)
  , ProjectRevision(..)
  , ProjectMetadata(..)
  , FossaError(..)
  , FossaReq(..)
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

  , getAttribution
  ) where

import App.Fossa.Analyze.Project
import qualified App.Fossa.Report.Attribution as Attr
import Control.Effect.Diagnostics
import Data.Coerce (coerce)
import Data.List (isInfixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Aeson
import Prelude
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Effect.Logger
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Req
import qualified Network.HTTP.Types as HTTP
import Path
import Srclib.Converter (toSourceUnit)
import Srclib.Types
import Text.URI (URI)
import qualified Text.URI as URI
import Data.Maybe (catMaybes)

newtype FossaReq m a = FossaReq { unFossaReq :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, Algebra sig)

instance (MonadIO m, Has Diagnostics sig m) => MonadHttp (FossaReq m) where
  handleHttpException = FossaReq . fatal . mangleError

-- parse a URI for use as a (base) Url, along with some default Options (e.g., port)
parseUri :: Has Diagnostics sig m => URI -> m (Url 'Https, Option 'Https)
parseUri uri = case useURI uri of
  Nothing -> fatalText ("Invalid URL: " <> URI.render uri)
  Just (Left (url, options)) -> pure (coerce url, coerce options)
  Just (Right (url, options)) -> pure (url, options)

fossaReq :: FossaReq m a -> m a
fossaReq = unFossaReq

-- TODO: git commit?
cliVersion :: Text
cliVersion = "spectrometer"

uploadUrl :: Url scheme -> Url scheme
uploadUrl baseurl = baseurl /: "api" /: "builds" /: "custom"

-- FIXME: we only want to include organizationId for "archive" and "custom"
-- TODO: need to normalize "git" projects
-- render a locator for use in fossa API urls
renderLocatorUrl :: Int -> Locator -> Text
renderLocatorUrl orgId Locator{..} =
  locatorFetcher <> "+" <> T.pack (show orgId) <> "/" <> locatorProject <> "$" <> fromMaybe "" locatorRevision

data UploadResponse = UploadResponse
  { uploadLocator :: Text
  , uploadError   :: Maybe Text
  } deriving (Eq, Ord, Show)

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

data ProjectRevision = ProjectRevision
  { projectName :: Text
  , projectRevision :: Text
  , projectBranch :: Text
  } deriving (Eq, Ord, Show)

data ProjectMetadata = ProjectMetadata
  { projectTitle :: Maybe Text
  , projectUrl :: Maybe Text
  , projectJiraKey :: Maybe Text
  , projectLink :: Maybe Text
  , projectTeam :: Maybe Text
  , projectPolicy :: Maybe Text
  } deriving (Eq, Ord, Show)

uploadAnalysis
  :: (Has Diagnostics sig m, MonadIO m)
  => Path Abs Dir -- ^ root directory for analysis
  -> URI -- ^ base url
  -> Text -- ^ api key
  -> ProjectRevision
  -> ProjectMetadata
  -> [Project]
  -> m UploadResponse
uploadAnalysis rootDir baseUri key ProjectRevision{..} metadata projects = fossaReq $ do
  (baseUrl, baseOptions) <- parseUri baseUri

  -- For each of the projects, we need to strip the root directory path from the prefix of the project path.
  -- We don't want parent directories of the scan root affecting "production path" filtering -- e.g., if we're
  -- running in a directory called "tmp", we still want results.
  let rootPath = fromAbsDir rootDir
      dropPrefix :: String -> String -> String
      dropPrefix prefix str = fromMaybe prefix (stripPrefix prefix str)
      filteredProjects = filter (isProductionPath . dropPrefix rootPath . fromAbsDir . projectPath) projects
     
      sourceUnits = fromMaybe [] $ traverse toSourceUnit filteredProjects
      opts = "locator" =: renderLocator (Locator "custom" projectName (Just projectRevision))
          <> "v" =: cliVersion
          <> "managedBuild" =: True
          <> "title" =: fromMaybe projectName (projectTitle metadata)
          <> "branch" =: projectBranch
          <> header "Authorization" ("token " <> encodeUtf8 key)
          <> mkMetadataOpts metadata
  resp <- req POST (uploadUrl baseUrl) (ReqBodyJson sourceUnits) jsonResponse (baseOptions <> opts)
  pure (responseBody resp)

mkMetadataOpts :: ProjectMetadata -> Option scheme
mkMetadataOpts ProjectMetadata{..} = mconcat $ catMaybes 
  [ ("projectURL" =:) <$> projectUrl
  , ("jiraProjectKey" =:) <$> projectJiraKey
  , ("link" =:) <$> projectLink
  , ("team" =:) <$> projectTeam
  , ("policy" =:) <$> projectPolicy
  ]

mangleError :: HttpException -> FossaError
mangleError err = case err of
  VanillaHttpException (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException resp _)) ->
    case HTTP.responseStatus resp of
      HTTP.Status 404 _ -> InvalidProjectOrRevision err
      HTTP.Status 403 _ -> NoPermission err
      _                 -> OtherError err
  JsonHttpException msg -> JsonDeserializeError msg
  _ -> OtherError err

isProductionPath :: FilePath -> Bool
isProductionPath path = not $ any (`isInfixOf` path)
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
  , "Carthage/"
  , "Checkouts/"
  ]

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
  } deriving (Eq, Ord, Show)

newtype BuildTask = BuildTask
  { buildTaskStatus :: BuildStatus
  } deriving (Eq, Ord, Show)

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
  :: (Has Diagnostics sig m, MonadIO m)
  => URI
  -> Text -- ^ api key
  -> Text -- ^ project name
  -> Text -- ^ project revision
  -> m Build
getLatestBuild baseUri key projectName projectRevision = fossaReq $ do
  (baseUrl, baseOptions) <- parseUri baseUri

  let opts = baseOptions <> header "Authorization" ("token " <> encodeUtf8 key)

  Organization orgId <- responseBody <$> req GET (organizationEndpoint baseUrl) NoReqBody jsonResponse opts
 
  response <- req GET (buildsEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse opts
  pure (responseBody response)

----------

issuesEndpoint :: Url 'Https -> Int -> Locator -> Url 'Https
issuesEndpoint baseUrl orgId locator = baseUrl /: "api" /: "cli" /: renderLocatorUrl orgId locator /: "issues"

getIssues
  :: (Has Diagnostics sig m, MonadIO m)
  => URI
  -> Text -- ^ api key
  -> Text -- ^ project name
  -> Text -- ^ project revision
  -> m Issues
getIssues baseUri key projectName projectRevision = fossaReq $ do
  (baseUrl, baseOptions) <- parseUri baseUri
 
  let opts = baseOptions <> header "Authorization" ("token " <> encodeUtf8 key)

  Organization orgId <- responseBody <$> req GET (organizationEndpoint baseUrl) NoReqBody jsonResponse opts
  response <- req GET (issuesEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse opts
  pure (responseBody response)

data Issues = Issues
  { issuesCount :: Int
  , issuesIssues :: [Issue]
  , issuesStatus :: Text
  } deriving (Eq, Ord, Show)

data IssueType
  = IssuePolicyConflict
  | IssuePolicyFlag
  | IssueVulnerability
  | IssueUnlicensedDependency
  | IssueOutdatedDependency
  | IssueOther Text
  deriving (Eq, Ord, Show)

renderIssueType :: IssueType -> Text
renderIssueType = \case
  IssuePolicyConflict -> "Denied by Policy"
  IssuePolicyFlag -> "Flagged by Policy"
  IssueVulnerability -> "Vulnerability"
  IssueUnlicensedDependency -> "Unlicensed Dependency"
  IssueOutdatedDependency -> "Outdated Dependency"
  IssueOther other -> other

data Issue = Issue
  { issueId :: Int
  , issuePriorityString :: Maybe Text -- we only use this field for `fossa test --json`
  , issueResolved :: Bool
  , issueRevisionId :: Text
  , issueType :: IssueType
  , issueRule :: Maybe IssueRule
  } deriving (Eq, Ord, Show)

newtype IssueRule = IssueRule
  { ruleLicenseId :: Maybe Text
  } deriving (Eq, Ord, Show)

instance FromJSON Issues where
  parseJSON = withObject "Issues" $ \obj ->
    Issues <$> obj .: "count"
           <*> obj .: "issues"
           <*> obj .: "status"

instance ToJSON Issues where
  toJSON Issues{..} = object
    [ "count" .= issuesCount
    , "issues" .= issuesIssues
    , "status" .= issuesStatus
    ]

instance FromJSON Issue where
  parseJSON = withObject "Issue" $ \obj ->
    Issue <$> obj .: "id"
           <*> obj .:? "priorityString"
           <*> obj .: "resolved"
           <*> obj .: "revisionId"
           <*> obj .: "type"
           <*> obj .:? "rule"

instance ToJSON Issue where
  toJSON Issue{..} = object
    [ "id" .= issueId
    , "priorityString" .= issuePriorityString
    , "resolved" .= issueResolved
    , "revisionId" .= issueRevisionId
    , "type" .= issueType
    , "rule" .= issueRule
    ]

instance FromJSON IssueType where
  parseJSON = withText "IssueType" $ \case
    "policy_conflict" -> pure IssuePolicyConflict
    "policy_flag" -> pure IssuePolicyFlag
    "vulnerability" -> pure IssueVulnerability
    "unlicensed_dependency" -> pure IssueUnlicensedDependency
    "outdated_dependency" -> pure IssueOutdatedDependency
    other -> pure (IssueOther other)

instance ToJSON IssueType where
  toJSON = String . \case
    IssuePolicyConflict -> "policy_conflict"
    IssuePolicyFlag -> "policy_flag"
    IssueVulnerability -> "vulnerability"
    IssueUnlicensedDependency -> "unlicensed_dependency"
    IssueOutdatedDependency -> "outdated_dependency"
    IssueOther text -> text

instance FromJSON IssueRule where
  parseJSON = withObject "IssueRule" $ \obj ->
    IssueRule <$> obj .:? "licenseId"

instance ToJSON IssueRule where
  toJSON IssueRule{..} = object ["licenseId" .= ruleLicenseId]

---------------

attributionEndpoint :: Url 'Https -> Int -> Locator -> Url 'Https
attributionEndpoint baseurl orgId locator = baseurl /: "api" /: "revisions" /: renderLocatorUrl orgId locator /: "attribution" /: "json"

getAttribution
  :: (Has Diagnostics sig m, MonadIO m)
  => URI
  -> Text -- ^ api key
  -> Text -- ^ project name
  -> Text -- ^ project revision
  -> m Attr.Attribution
getAttribution baseUri key project revision = fossaReq $ do
  (baseUrl, baseOptions) <- parseUri baseUri

  let opts = baseOptions
        <> header "Authorization" ("token " <> encodeUtf8 key)
        <> "includeDeepDependencies" =: True
        <> "includeHashAndVersionData" =: True
        <> "includeDownloadUrl" =: True
  Organization orgId <- responseBody <$> req GET (organizationEndpoint baseUrl) NoReqBody jsonResponse opts
  response <- req GET (attributionEndpoint baseUrl orgId (Locator "custom" project (Just revision))) NoReqBody jsonResponse opts
  pure (responseBody response)

----------

newtype Organization = Organization
  { organizationId :: Int
  } deriving (Eq, Ord, Show)

instance FromJSON Organization where
  parseJSON = withObject "Organization" $ \obj ->
    Organization <$> obj .: "organizationId"

organizationEndpoint :: Url scheme -> Url scheme
organizationEndpoint baseurl = baseurl /: "api" /: "cli" /: "organization"
