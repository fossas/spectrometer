module Strategy.Mix (
  discover,
  findProjects,
  getDeps,
  mkProject,
) where

import Control.Effect.Diagnostics (Diagnostics, context, (<||>))
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipSome),
  findFileNamed,
  walk',
 )
import Effect.Exec (Exec, Has)
import Effect.Logger (Logger (..))
import Effect.ReadFS (ReadFS)
import Graphing (Graphing)
import Path
import Strategy.Elixir.MixExs qualified as MixExs
import Strategy.Elixir.MixTree qualified as MixTree
import Types (Dependency, DiscoveredProject (..), GraphBreadth (..))

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has Logger rsig run, Has ReadFS rsig run, Has Exec rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = context "Mix" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [MixProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "mix.exs" files of
    Nothing -> pure ([], WalkContinue)
    Just exsFile -> pure ([MixProject dir exsFile], WalkSkipSome ["deps", "_build"])

data MixProject = MixProject
  { mixDir :: Path Abs Dir
  , exsFile :: Path Abs File
  }
  deriving (Eq, Ord, Show)

mkProject :: (Has ReadFS sig n, Has Exec sig n, Has Diagnostics sig n, Has Logger sig n) => MixProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "mix"
    , projectBuildTargets = mempty
    , projectDependencyGraph = const $ getDeps project
    , projectPath = mixDir project
    , projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m, Has Logger sig m) => MixProject -> m (Graphing Dependency, GraphBreadth)
getDeps project = MixTree.analyze (mixDir project) <||> MixExs.analyze (exsFile project)
