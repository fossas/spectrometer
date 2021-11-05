module Strategy.Rebar3 (
  discover,
  findProjects,
  getDeps,
  mkProject,
) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import Control.Effect.Diagnostics (Diagnostics, context)
import Data.Aeson (ToJSON)
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipAll),
  findFileNamed,
  walk',
 )
import Effect.Exec (Exec, Has)
import Effect.ReadFS (ReadFS)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path)
import Strategy.Erlang.Rebar3Tree qualified as Rebar3Tree
import Types (
  DependencyResults (
    DependencyResults,
    dependencyGraph,
    dependencyGraphBreadth,
    dependencyManifestFiles
  ),
  DiscoveredProject (
    DiscoveredProject,
    projectBuildTargets,
    projectData,
    projectPath,
    projectType
  ),
 )

discover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [DiscoveredProject RebarProject]
discover dir = context "Rebar3" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [RebarProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "rebar.config" files of
    Nothing -> pure ([], WalkContinue)
    Just f -> pure ([RebarProject dir f], WalkSkipAll)

data RebarProject = RebarProject
  { rebarDir :: Path Abs Dir
  , rebarFile :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON RebarProject

instance AnalyzeProject RebarProject where
  analyzeProject _ = getDeps

mkProject :: RebarProject -> DiscoveredProject RebarProject
mkProject project =
  DiscoveredProject
    { projectType = "rebar3"
    , projectBuildTargets = mempty
    , projectPath = rebarDir project
    , projectData = project
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => RebarProject -> m DependencyResults
getDeps project = do
  (graph, graphBreadth) <- Rebar3Tree.analyze' (rebarDir project)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [rebarFile project]
      }
