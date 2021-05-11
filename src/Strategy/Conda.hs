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

mkProject :: (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m) => CondaProject -> DiscoveredProject m
mkProject project =
  DiscoveredProject
    { projectType = "conda",
      projectBuildTargets = mempty,
      projectDependencyGraph = const $ getDeps project, -- what does const do here?
      projectPath = condaDir project,
      projectLicenses = pure []
    }

-- Prefer analyzeCondaList over analyzeEnvironmentYml, results shoudln't be combined, it's either/or.
-- There might be a dep with a version spec in an environment.yml file: i.e. conda+foo$1.2.*, and perhaps
-- the same dep resolved to a known version in the users virtual environment: i.e. conda+foo$1.2.4 (we get that form conda list).
-- If we combined the results then we would include both of those deps in the result, which is not correct behavior.
getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => CondaProject -> m (Graphing Dependency)
getDeps project = analyzeCondaList project <||> analyzeEnvironmentYml project

analyzeCondaList :: (Has Exec sig m, Has Diagnostics sig m) => CondaProject -> m (Graphing Dependency)
analyzeCondaList = CondaList.analyze' . condaDir

analyzeEnvironmentYml :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => CondaProject -> m (Graphing Dependency)
analyzeEnvironmentYml = EnvironmentYml.analyze' . condaEnvironmentYml
