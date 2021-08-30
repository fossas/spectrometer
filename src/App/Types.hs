module App.Types (
  BaseDir (..),
  NinjaGraphCLIOptions (..),
  OverrideProject (..),
  ProjectMetadata (..),
  ReleaseGroupMetadata (..),
  ProjectRevision (..),
  MonorepoAnalysisOpts (..),
  FeatureFlag (..),
  coreFlagName,
) where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Text (Text)
import Path (Abs, Dir, Path)

newtype BaseDir = BaseDir {unBaseDir :: Path Abs Dir} deriving (Eq, Ord, Show)

data OverrideProject = OverrideProject
  { overrideName :: Maybe Text
  , overrideRevision :: Maybe Text
  , overrideBranch :: Maybe Text
  }

data ProjectMetadata = ProjectMetadata
  { projectTitle :: Maybe Text
  , projectUrl :: Maybe Text
  , projectJiraKey :: Maybe Text
  , projectLink :: Maybe Text
  , projectTeam :: Maybe Text
  , projectPolicy :: Maybe Text
  , projectReleaseGroup :: Maybe ReleaseGroupMetadata
  }
  deriving (Eq, Ord, Show)

data ReleaseGroupMetadata = ReleaseGroupMetadata
  { releaseGroupName :: Text
  , releaseGroupRelease :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON ReleaseGroupMetadata where
  parseJSON = withObject "ReleaseGroupMetadata" $ \obj ->
    ReleaseGroupMetadata <$> obj .: "name"
      <*> obj .: "release"

newtype MonorepoAnalysisOpts = MonorepoAnalysisOpts
  { monorepoAnalysisType :: Maybe Text
  }

data ProjectRevision = ProjectRevision
  { projectName :: Text
  , projectRevision :: Text
  , projectBranch :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data NinjaGraphCLIOptions = NinjaGraphCLIOptions
  { ninjaBaseDir :: FilePath
  , ninjaDepsFile :: Maybe FilePath
  , ninjaLunchTarget :: Maybe Text
  , ninjaScanId :: Text
  , ninjaBuildName :: Text
  }

-- | Feature flags are set in the FOSSA API.
-- Not all feature flags are relevant to Spectrometer, but those that are can be found here.
--
-- Since this is client side, the intent is to use the feature flag state to provide better errors to users,
-- or for FOSSA to selectively enable experimental or optional features in CLI on an organization's behalf org-wide.
data FeatureFlag
  = -- | Enable VSI and Monorepo functionality
    FeatureFlagVSIMonorepo

-- | Translate the flag to its name in the FOSSA API.
coreFlagName :: FeatureFlag -> Text
coreFlagName FeatureFlagVSIMonorepo = "vendoredPackageScanning"
