module Strategy.Rebar3 (
  discover,
  findProjects,
  getDeps,
  mkProject,
) where

import Control.Effect.Diagnostics (Diagnostics, context)
import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import Path
import Strategy.Erlang.Rebar3Tree qualified as Rebar3Tree
import Types

-- discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Exec rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
-- discover dir = context "Rebar3" $ do
--   projects <- context "Finding projects" $ findProjects dir
--   pure (map mkProject projects)
discover = undefined
mkProject = undefined

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [RebarProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "rebar.config" files of
    Nothing -> pure ([], WalkContinue)
    Just f -> pure ([RebarProject dir f], WalkSkipAll)

data RebarProject = RebarProject
  { rebarDir :: Path Abs Dir
  , rebarFile :: Path Abs File
  }
  deriving (Eq, Ord, Show)

-- mkProject :: (Has Exec sig n, Has ReadFS sig n, Has Diagnostics sig n) => RebarProject -> DiscoveredProject n
-- mkProject project =
--   DiscoveredProject
--     { projectType = "rebar3"
--     , projectBuildTargets = mempty
--     , projectDependencyResults = const $ getDeps project
--     , projectPath = rebarDir project
--     , projectLicenses = pure []
--     }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => RebarProject -> m DependencyResults
getDeps project = do
  (graph, graphBreadth) <- Rebar3Tree.analyze' (rebarDir project)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [rebarFile project]
      }
