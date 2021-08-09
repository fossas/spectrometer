module Strategy.Shards (
  discover,
  findProjects,
  getDeps,
  mkProject,
) where

import Control.Effect.Diagnostics (Diagnostics, context)
import Discovery.Walk (WalkStep (WalkContinue, WalkSkipAll), findFileNamed, walk')
import Effect.Exec (Exec, Has)
import Effect.ReadFS (ReadFS)
import Graphing (Graphing)
import Path
import Strategy.Crystal.ShardYml (analyzeShardYmlFile)
import Types (Dependency, DiscoveredProject (..), GraphBreadth)

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Exec rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = context "shards" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [ShardProject]
findProjects = walk' $ \dir _ files -> do
  case findFileNamed "shard.yml" files of
    Nothing -> pure ([], WalkContinue)
    Just shardYmlFile -> pure ([ShardProject shardYmlFile dir], WalkSkipAll)

data ShardProject = ShardProject
  { shardYml :: Path Abs File
  , shardDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show)

mkProject :: (Has Exec sig n, Has ReadFS sig n, Has Diagnostics sig n) => ShardProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "shard"
    , projectBuildTargets = mempty
    , projectDependencyGraph = const $ getDeps project
    , projectPath = shardDir project
    , projectLicenses = pure []
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => ShardProject -> m (Graphing Dependency, GraphBreadth)
getDeps project = analyzeShardYmlFile $ shardYml project
