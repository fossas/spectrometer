{-# LANGUAGE ScopedTypeVariables #-}

module Strategy.Maven
  ( discover,
    mkProject,
    getDeps,
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
import qualified Strategy.Maven.Pom.Closure as PomClosure
import Types

discover ::
  ( MonadIO m,
    Has (Lift IO) sig m,
    Has Diagnostics sig m,
    Has ReadFS sig m,
    Has Exec sig' n,
    Has ReadFS sig' n,
    Has Diagnostics sig' n,
    Has (Lift IO) sig' n
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject n]
discover dir = map (mkProject dir) <$> PomClosure.findProjects dir

mkProject ::
  (Has ReadFS sig n, Has Exec sig n, Has (Lift IO) sig n, Has Diagnostics sig n) =>
  -- | basedir; required for licenses
  Path Abs Dir -> PomClosure.MavenProjectClosure -> DiscoveredProject n
mkProject basedir closure = 
  DiscoveredProject
    { projectType = "maven",
      projectPath = parent $ PomClosure.closurePath closure,
      projectBuildTargets = mempty,
      projectDependencyGraph = const $ getDeps closure,
      projectLicenses = pure $ Pom.getLicenses basedir closure
    }

getDeps ::
  ( Has (Lift IO) sig m,
    Has Diagnostics sig m,
    Has ReadFS sig m,
    Has Exec sig m
  ) =>
  PomClosure.MavenProjectClosure ->
  m (Graphing Dependency)
getDeps closure = Plugin.analyze' (parent (PomClosure.closurePath closure)) <||> pure (Pom.analyze' closure)
