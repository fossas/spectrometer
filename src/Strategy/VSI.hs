module Strategy.VSI
  ( discover
  ) where

import Control.Effect.Diagnostics
import Effect.Exec
import Effect.ReadFS (ReadFS)
import Path
import Types
import Graphing
import Fossa.API.Types

newtype VSIProject = VSIProject
  { vsiDir :: Path Abs Dir
  } deriving (Eq, Ord, Show)

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has Exec rsig run, Has Diagnostics rsig run) => ApiOpts -> Path Abs Dir -> m [DiscoveredProject run]
discover _ dir = context "VSI" $ do
  -- Right now we assume that if the VSI strategy is run, the top level root is a project to scan.
  -- In the future we will likely add heuristics to detect whether the VSI strategy will yield results.
  pure [mkProject (VSIProject dir)]

mkProject :: (Has Exec sig n, Has Diagnostics sig n) => VSIProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "vsi"
    , projectBuildTargets = mempty
    , projectDependencyGraph = const $ analyze (vsiDir project)
    , projectPath = vsiDir project
    , projectLicenses = pure []
    }

analyze :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m (Graphing Dependency)
analyze _ = pure empty
