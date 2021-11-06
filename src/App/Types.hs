{-# LANGUAGE RecordWildCards #-}

module App.Types (
  BaseDir (..),
  NinjaGraphCLIOptions (..),
  OverrideProject (..),
  ProjectMetadata (..),
  ReleaseGroupMetadata (..),
  ProjectRevision (..),
  MonorepoAnalysisOpts (..),
) where

import Control.Effect.Record (RecordableValue)
import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), object, withObject, (.:))
import Data.Aeson.Types (ToJSON (toJSON))
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

instance ToJSON ProjectMetadata where
  toJSON ProjectMetadata{..} =
    object
      [ "projectTitle" .= projectTitle
      , "projectUrl" .= projectUrl
      , "projectJiraKey" .= projectJiraKey
      , "projectLink" .= projectLink
      , "projectTeam" .= projectTeam
      , "projectPolicy" .= projectPolicy
      , "projectReleaseGroup" .= projectReleaseGroup
      ]

instance RecordableValue (ProjectMetadata)

data ReleaseGroupMetadata = ReleaseGroupMetadata
  { releaseGroupName :: Text
  , releaseGroupRelease :: Text
  }
  deriving (Eq, Ord, Show)

instance ToJSON ReleaseGroupMetadata where
  toJSON ReleaseGroupMetadata{..} =
    object
      [ "releaseGroupName" .= releaseGroupName
      , "releaseGroupRelease" .= releaseGroupRelease
      ]

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

instance ToJSON ProjectRevision where
  toJSON ProjectRevision{..} =
    object
      [ "projectName" .= projectName
      , "projectRevision" .= projectRevision
      , "projectBranch" .= projectBranch
      ]

instance RecordableValue (ProjectRevision)

data NinjaGraphCLIOptions = NinjaGraphCLIOptions
  { ninjaBaseDir :: FilePath
  , ninjaDepsFile :: Maybe FilePath
  , ninjaLunchTarget :: Maybe Text
  , ninjaScanId :: Text
  , ninjaBuildName :: Text
  }
