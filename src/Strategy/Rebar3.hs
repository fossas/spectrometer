module Strategy.Rebar3
  ( discover',
  )
where

import Control.Effect.Diagnostics (Diagnostics)
import Control.Monad.IO.Class
import Data.List (find)
import Discovery.Walk
import Effect.Exec
import Graphing
import Path
import qualified Strategy.Erlang.Rebar3Tree as Rebar3Tree
import Types

discover' ::
  ( MonadIO m,
    Has Exec sig m,
    Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m [NewProject m]
discover' dir = map mkProject <$> findProjects dir

findProjects :: MonadIO m => Path Abs Dir -> m [RebarProject]
findProjects = walk' $ \dir _ files -> do
  case find (\f -> fileName f == "rebar.config") files of
    Nothing -> pure ([], WalkContinue)
    Just _ -> pure ([RebarProject dir], WalkSkipAll)

data RebarProject = RebarProject
  { rebarDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show)

mkProject :: (Has Exec sig m, Has Diagnostics sig m) => RebarProject -> NewProject m
mkProject project =
  NewProject
    { projectType = "rebar3",
      projectBuildTargets = mempty,
      projectDependencyGraph = getDeps project,
      projectPath = rebarDir project,
      projectLicenses = pure []
    }

getDeps :: (Has Exec sig m, Has Diagnostics sig m) => RebarProject -> m (Graphing Dependency)
getDeps project = Rebar3Tree.analyze' (rebarDir project)
