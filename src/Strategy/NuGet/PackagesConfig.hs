{-# LANGUAGE RecordWildCards #-}

module Strategy.NuGet.PackagesConfig
  ( discover
  , discover'
  , buildGraph
  , analyze

  , PackagesConfig(..)
  , NuGetDependency(..)
  ) where

import Control.Effect.Diagnostics
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (find)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing)
import qualified Graphing
import Parse.XML
import Path
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> (fileName f) == "packages.config") files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "nuget-packagesconfig" DotnetGroup $ analyze file

  pure WalkContinue

discover' :: MonadIO m => Path Abs Dir -> m [NewProject]
discover' dir = map mkProject <$> findProjects dir

findProjects :: MonadIO m => Path Abs Dir -> m [PackagesConfigProject]
findProjects = walk' $ \_ _ files -> do
  case find (\f -> fileName f == "packages.config") files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([PackagesConfigProject file], WalkContinue)

data PackagesConfigProject = PackagesConfigProject
  { packagesConfigFile :: Path Abs File
  }
  deriving (Eq, Ord, Show)

mkProject :: PackagesConfigProject -> NewProject
mkProject project =
  NewProject
    { projectType = "packagesconfig",
      projectBuildTargets = mempty,
      projectDependencyGraph = const . runReadFSIO $ getDeps project,
      projectPath = parent $ packagesConfigFile project,
      projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => PackagesConfigProject -> m (Graphing Dependency)
getDeps = analyze' . packagesConfigFile

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze' file = buildGraph <$> readContentsXML file

instance FromXML PackagesConfig where
  parseElement el = PackagesConfig <$> children "package" el

instance FromXML NuGetDependency where
  parseElement el =
    NuGetDependency <$> attr "id" el
                    <*> attr "version" el

newtype PackagesConfig = PackagesConfig
  { deps :: [NuGetDependency]
  } deriving (Eq, Ord, Show)

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m ProjectClosureBody
analyze file = mkProjectClosure file <$> readContentsXML file

mkProjectClosure :: Path Abs File -> PackagesConfig -> ProjectClosureBody
mkProjectClosure file config = ProjectClosureBody
  { bodyModuleDir    = parent file
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph config
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

data NuGetDependency = NuGetDependency
  { depID      :: Text
  , depVersion :: Text
  } deriving (Eq, Ord, Show)

buildGraph :: PackagesConfig -> Graphing Dependency
buildGraph = Graphing.fromList . map toDependency . deps
    where
    toDependency NuGetDependency{..} =
      Dependency { dependencyType = NuGetType
               , dependencyName = depID
               , dependencyVersion = Just (CEq depVersion)
               , dependencyLocations = []
               , dependencyEnvironments = []
               , dependencyTags = M.empty
               }
