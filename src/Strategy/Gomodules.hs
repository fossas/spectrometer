module Strategy.Gomodules
  ( discover',
  )
where

import Control.Effect.Diagnostics (Diagnostics, (<||>))
import Control.Monad.IO.Class
import Data.List (find)
import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import Graphing
import Path
import qualified Strategy.Go.GoList as GoList
import qualified Strategy.Go.Gomod as Gomod
import Types

discover' ::
  ( MonadIO m,
    Has Exec sig m,
    Has ReadFS sig m,
    Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m [NewProject m]
discover' dir = map mkProject <$> findProjects dir

findProjects :: MonadIO m => Path Abs Dir -> m [GomodulesProject]
findProjects = walk' $ \dir _ files -> do
  case find (\f -> fileName f == "go.mod") files of
    Nothing -> pure ([], WalkSkipSome ["vendor"])
    Just gomod -> pure ([GomodulesProject gomod dir], WalkSkipSome ["vendor"])

data GomodulesProject = GomodulesProject
  { gomodulesGomod :: Path Abs File,
    gomodulesDir :: Path Abs Dir
  }

mkProject :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => GomodulesProject -> NewProject m
mkProject project =
  NewProject
    { projectType = "gomod",
      projectDependencyGraph = getDeps project,
      projectPath = gomodulesDir project,
      projectLicenses = pure []
    }

getDeps :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => GomodulesProject -> m (Graphing Dependency)
getDeps project =
  GoList.analyze' (gomodulesDir project)
    <||> Gomod.analyze' (gomodulesGomod project)
