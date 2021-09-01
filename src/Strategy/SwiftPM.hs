module Strategy.SwiftPM (
  discover,
  findProjects,
  mkProject,
) where

import Control.Carrier.Simple (Has)
import Control.Effect.Diagnostics (Diagnostics, context)
import Discovery.Walk (
  WalkStep (WalkContinue),
  findFileNamed,
  walk',
 )
import Effect.ReadFS (ReadFS)
import Path
import Strategy.Swift.PackageSwift (analyzePackageSwift)
import Types (DependencyResults (..), DiscoveredProject (..))

data SwiftPackageProject = SwiftPackageProject
  { manifest :: Path Abs File
  , projectDir :: Path Abs Dir
  , resolved :: Maybe (Path Abs File)
  }
  deriving (Show, Eq, Ord)

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = context "Swift" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [SwiftPackageProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "Package.swift" files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([SwiftPackageProject file dir Nothing], WalkContinue)

mkProject :: (Has ReadFS sig n, Has Diagnostics sig n) => SwiftPackageProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "swift"
    , projectBuildTargets = mempty
    , projectDependencyResults = const $ getDeps project
    , projectPath = projectDir project
    , projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => SwiftPackageProject -> m DependencyResults
getDeps project = do
  (graph, graphBreadth) <- analyzePackageSwift $ manifest project
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = graphBreadth
      , dependencyManifestFiles = [manifest project]
      }
