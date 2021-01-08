{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Strategy.NuGet.ProjectJson
  ( discover
  , findProjects
  , getDeps
  , mkProject
  , buildGraph

  , ProjectJson(..)
  ) where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson.Types
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing)
import qualified Graphing
import Path
import Types

discover :: MonadIO m => Path Abs Dir -> m [DiscoveredProject]
discover dir = map mkProject <$> findProjects dir

findProjects :: MonadIO m => Path Abs Dir -> m [ProjectJsonProject]
findProjects = walk' $ \_ _ files -> do
  case findFileNamed "project.json" files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([ProjectJsonProject file], WalkContinue)

data ProjectJsonProject = ProjectJsonProject
  { projectJsonFile :: Path Abs File
  }
  deriving (Eq, Ord, Show)

mkProject :: ProjectJsonProject -> DiscoveredProject
mkProject project =
  DiscoveredProject
    { projectType = "projectjson",
      projectBuildTargets = mempty,
      projectDependencyGraph = const . runReadFSIO $ getDeps project,
      projectPath = parent $ projectJsonFile project,
      projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => ProjectJsonProject -> m (Graphing Dependency)
getDeps = analyze' . projectJsonFile

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze' file = buildGraph <$> readContentsJson @ProjectJson file

data ProjectJson = ProjectJson
  { dependencies     :: Map Text DependencyInfo
  } deriving Show

data DependencyInfo = DependencyInfo
  { depVersion    :: Text
  , depType       :: Maybe Text
  } deriving Show

instance FromJSON ProjectJson where
  parseJSON = withObject "ProjectJson" $ \obj ->
    ProjectJson <$> obj .: "dependencies"

instance FromJSON DependencyInfo where
  parseJSON val = parseJSONObject val <|> parseJSONText val
    where
    parseJSONObject :: Value -> Parser DependencyInfo
    parseJSONObject = withObject "DependencyInfo" $ \obj ->
        DependencyInfo <$> obj .: "version"
                        <*> obj .:? "type"

    parseJSONText :: Value -> Parser DependencyInfo
    parseJSONText = withText "DependencyVersion" $ \text ->
        pure $ DependencyInfo text Nothing

data NuGetDependency = NuGetDependency
  { name            :: Text
  , version         :: Text
  , dependencyType  :: Maybe Text
  } deriving Show

buildGraph :: ProjectJson -> Graphing Dependency
buildGraph project = Graphing.fromList (map toDependency direct)
    where
    direct = (\(name, dep) -> NuGetDependency name (depVersion dep) (depType dep)) <$> M.toList (dependencies project)
    toDependency NuGetDependency{..} =
      Dependency { dependencyType = NuGetType
               , dependencyName = name
               , dependencyVersion = case T.find ('*' ==) version of
                  Just '*' -> Just (CCompatible version)
                  _ -> Just (CEq version)
               , dependencyLocations = []
               , dependencyEnvironments = []
               , dependencyTags = case dependencyType of
                  Nothing -> M.empty
                  Just depType -> M.insert "type" [depType]  M.empty
               }
