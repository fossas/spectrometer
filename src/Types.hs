module Types
  ( StrategyGroup(..)

  , ProjectClosure(..)
  , StrategyGroup'(..)

  , Optimal(..)
  , Complete(..)

  , ProjectDependencies(..)
  , TaskEffs

  , HasDiscover
  , runStrategy
  , runSimpleStrategy
  ) where

import Prologue

import Control.Algebra
import Control.Carrier.Error.Either
import Control.Carrier.TaskPool
import Control.Effect.Exception
import Control.Effect.Output
import DepTypes
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Graphing

---------- Discovery

runSimpleStrategy ::
  ( Has (Lift IO) sig m
  , Has TaskPool sig m
  , Has (Output ProjectClosure) sig m
  )
  => Text -> StrategyGroup' -> TaskC m ProjectClosure -> m ()
runSimpleStrategy _ _ act = forkTask $ do
  let runIt = runError @ReadFSErr
            . runError @ExecErr
            . runReadFSIO
            . runExecIO

  mask $ \restore -> do
    (res :: Either SomeException a) <- try (restore (runIt act))
    case res of
      Left _ -> pure () -- TODO
      Right (Left _) -> pure () -- TODO
      Right (Right (Left _)) -> pure () -- TODO
      Right (Right (Right a)) -> output a

runStrategy ::
  ( Has (Lift IO) sig m
  , Has TaskPool sig m
  )
  => Text -> StrategyGroup' -> TaskC m () -> m ()
runStrategy _ _ act = forkTask $ do
  let runIt = runError @ReadFSErr
            . runError @ExecErr
            . runReadFSIO
            . runExecIO

  mask $ \restore -> do
    (res :: Either SomeException a) <- try (restore (runIt act))
    case res of
      Left _ -> pure () -- TODO
      Right (Left _) -> pure () -- TODO
      Right (Right (Left _)) -> pure () -- TODO
      Right (Right (Right ())) -> pure ()

type TaskC m a = ExecIOC (ReadFSIOC (ErrorC ExecErr (ErrorC ReadFSErr m))) a

type HasDiscover sig m = (Has (Lift IO :+: Output ProjectClosure :+: TaskPool) sig m, MonadIO m, Effect sig)

-- | The effects available for use in Tasks
type TaskEffs sig m =
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , Has TaskPool sig m
  , Has (Output ProjectClosure) sig m
  , MonadIO m
  , Effect sig
  )

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
  } deriving (Eq, Ord, Show, Generic)

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
