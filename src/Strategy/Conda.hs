{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Strategy.Conda (
  discover,
) where

import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import Control.Carrier.Diagnostics hiding (fromMaybe)
import Path
import Types
import Graphing (Graphing)
import qualified Strategy.Conda.CondaList as CondaList
import qualified Strategy.Conda.EnvironmentYml as EnvironmentYml

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Exec rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = map mkProject <$> findProjects dir

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [CondaProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "environment.yml" files of
    Nothing -> pure([], WalkContinue)
    Just envYml -> do
      let project =
            CondaProject
              {
                condaDir = dir,
                condaEnvironmentYml = envYml
              }
      pure([project], WalkSkipAll) -- Once we find an environment.yml file, skip the rest of the walk

data CondaProject =
    CondaProject
      { condaDir :: Path Abs Dir,
        condaEnvironmentYml :: Path Abs File
      } deriving (Eq, Ord, Show)

mkProject :: (Has ReadFS sig n, Has Exec sig n, Has Diagnostics sig n) => CondaProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "conda",
      projectBuildTargets = mempty,
      projectDependencyGraph = const $ getDeps project, -- what does const do here?
      projectPath = condaDir project,
      projectLicenses = pure []
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => CondaProject -> m (Graphing Dependency)
getDeps project = analyzeCondaList project <||> analyzeEnvironmentYml project

analyzeCondaList :: (Has Exec sig m, Has Diagnostics sig m) => CondaProject -> m (Graphing Dependency)
analyzeCondaList = CondaList.analyze' . condaDir

analyzeEnvironmentYml :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => CondaProject -> m (Graphing Dependency)
analyzeEnvironmentYml = EnvironmentYml.analyze' . condaEnvironmentYml
