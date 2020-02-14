module Types
  ( Discover(..)
  , StrategyGroup(..)

  , ProjectClosure(..)
  , StrategyGroup'(..)

  , Optimal(..)
  , Complete(..)

  , ProjectDependencies(..)
  , Task(..)
  , TaskEffs
  , forkTask
  ) where

import Prologue

import Polysemy
import Polysemy.Error
import Polysemy.Output
import Polysemy.Resource

import DepTypes
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Diagnostics
import Graphing

---------- Discovery

data Task = Task
  { taskName :: Text
  , taskRun  :: forall r. TaskEffs r => Sem r ()
  }

-- | The effects available for use in Tasks
type TaskEffs r = Members '[Embed IO, Resource, Logger, Error CLIErr, Exec, Error ExecErr, ReadFS, Error ReadFSErr, Output Task, Output ProjectClosure] r

forkTask :: forall r r1. (TaskEffs r, Member (Output Task) r1) => Text -> Sem r () -> Sem r1 ()
forkTask name act = output @Task (Task name act)

-- | Discover functions produce 'ConfiguredStrategy's, given a base directory
-- to search
data Discover = Discover
  { discoverName :: Text
  , discoverFunc :: forall r. TaskEffs r => Path Abs Dir -> Sem r ()
  }

---------- Strategies

-- FUTURE: determine strategy completeness during scan?

data Optimal = Optimal | NotOptimal
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Optimal where
  toJSON Optimal    = toJSON True
  toJSON NotOptimal = toJSON False

data Complete = Complete | NotComplete
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Complete where
  toJSON Complete    = toJSON True
  toJSON NotComplete = toJSON False

-- TODO: typeclassify?

data ProjectClosure = ProjectClosure
  { closureStrategyGroup :: StrategyGroup'
  , closureStrategyName  :: Text -- ^ e.g., "python-pipenv". This is temporary: ProjectClosures will eventually combine several strategies into one
  , closureModuleDir     :: Path Rel Dir -- ^ the relative module directory (for grouping with other strategies)
  , closureDependencies  :: ProjectDependencies
  }

data ProjectDependencies = ProjectDependencies
  { dependenciesGraph    :: Graphing Dependency
  , dependenciesOptimal  :: Optimal -- ^ Whether this dependency graph is considered "optimal" -- i.e., best case analysis for this given project. Notably, this __does not__ imply "complete".
  , dependenciesComplete :: Complete -- ^ Whether this dependency graph contains all dependencies for a given project. When @NotComplete@, the backend will run a hasGraph-like analysis to produce the missing dependencies and graph edges
  } deriving (Eq, Ord, Show, Generic)

data StrategyGroup' =
    CarthageGroup
  | DotnetGroup
  | GolangGroup
  | GradleGroup
  | MavenGroup
  | NodejsGroup
  | PythonGroup
  | RubyGroup
  deriving (Eq, Ord, Show, Generic)

-- | 'Strategy' outputs are grouped and sorted based on the provided @StrategyGroup@s
--
-- This is temporary: ProjectClosures will eventually supersede this behavior
--
-- For example, @"python"@ is a @StrategyGroup@ that has pipenv, piplist, ... as strategies
data StrategyGroup = StrategyGroup
  { groupName       :: Text -- ^ e.g., "python"
  , groupStrategies :: [Text]
  }
