module App.Fossa.Analyze.Project (
  ProjectResult (..),
  mkResult,
) where

import Data.Set qualified as S
import Data.Text (Text)
import DepTypes
import Graphing (Graphing)
import Graphing qualified
import Path
import Path.Extra (toRelativePath)
import Types

mkResult :: Path Abs Dir -> DiscoveredProject n -> (DependencyResults) -> ProjectResult
mkResult basedir project dependencyResults =
  ProjectResult
    { projectResultType = projectType project
    , projectResultPath = projectPath project
    , projectResultGraph =
        -- FIXME: this is a hack to work around analyzers that aren't able to
        -- determine which dependencies are direct. Without this hack, all of
        -- their dependencies would be filtered out. The real fix to this is to
        -- have a separate designation for "reachable" vs "direct" on nodes in a
        -- Graphing, where direct deps are inherently reachable.
        if S.null (Graphing.graphingDirect graph)
          then graph
          else Graphing.pruneUnreachable graph
    , projectResultGraphBreadth = dependencyGraphBreadth dependencyResults
    , projectResultManifestFiles = relativeManifestFiles
    }
  where
    graph = dependencyGraph dependencyResults
    relativeManifestFiles = map (toRelativePath basedir) $ dependencyManifestFiles dependencyResults

data ProjectResult = ProjectResult
  { projectResultType :: Text
  , projectResultPath :: Path Abs Dir
  , projectResultGraph :: Graphing Dependency
  , projectResultGraphBreadth :: GraphBreadth
  , projectResultManifestFiles :: [FilePath]
  }
