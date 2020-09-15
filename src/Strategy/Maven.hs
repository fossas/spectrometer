{-# LANGUAGE ScopedTypeVariables #-}

module Strategy.Maven
  ( discover,
  )
where

import Control.Effect.Diagnostics
import Control.Effect.Lift
import Control.Monad.IO.Class
import Effect.Exec
import Effect.ReadFS
import Graphing (Graphing)
import Path
import qualified Strategy.Maven.PluginStrategy as Plugin
import qualified Strategy.Maven.Pom as Pom
import qualified Strategy.Maven.Pom.Closure as Pom
import Types

discover ::
  ( MonadIO m,
    Has (Lift IO) sig m,
    Has Exec sig m,
    Has ReadFS sig m,
    Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m [NewProject m]
discover dir = map mkProject <$> Pom.findProjects dir

mkProject ::
  ( Has (Lift IO) sig m,
    Has Diagnostics sig m,
    Has ReadFS sig m,
    Has Exec sig m
  ) =>
  Pom.MavenProjectClosure ->
  NewProject m
mkProject closure = 
  NewProject
    { projectType = "Maven",
      projectPath = parent $ Pom.closurePath closure,
      projectDependencyGraph = getDeps closure,
      projectLicenses = undefined
    }

getDeps ::
  ( Has (Lift IO) sig m,
    Has Diagnostics sig m,
    Has ReadFS sig m,
    Has Exec sig m
  ) =>
  Pom.MavenProjectClosure ->
  m (Graphing Dependency)
getDeps closure = Plugin.analyze' (parent (Pom.closurePath closure)) `fallingBackTo` pure (Pom.analyze' closure)
