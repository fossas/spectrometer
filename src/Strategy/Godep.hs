module Strategy.Godep
  ( discover',
  )
where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, (<||>))
import qualified Control.Effect.Diagnostics as Diag
import Control.Monad.IO.Class
import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import Graphing
import Path
import qualified Strategy.Go.GopkgLock as GopkgLock
import qualified Strategy.Go.GopkgToml as GopkgToml
import Types

discover' :: MonadIO m => Path Abs Dir -> m [NewProject]
discover' dir = map mkProject <$> findProjects dir

findProjects :: MonadIO m => Path Abs Dir -> m [GodepProject]
findProjects = walk' $ \dir _ files -> do
  let gopkgToml = findFileNamed "Gopkg.toml" files
  let gopkgLock = findFileNamed "Gopkg.lock" files

  let project =
        GodepProject
          { godepToml = gopkgToml,
            godepLock = gopkgLock,
            godepDir = dir
          }

  case gopkgToml <|> gopkgLock of
    Nothing -> pure ([], WalkSkipSome ["vendor"])
    Just _ -> pure ([project], WalkSkipSome ["vendor"])

data GodepProject = GodepProject
  { godepDir :: Path Abs Dir,
    godepToml :: Maybe (Path Abs File),
    godepLock :: Maybe (Path Abs File)
  }

mkProject :: GodepProject -> NewProject
mkProject project =
  NewProject
    { projectType = "godep",
      projectBuildTargets = mempty,
      projectDependencyGraph = const . runReadFSIO . runExecIO $ getDeps project,
      projectPath = godepDir project,
      projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m) => GodepProject -> m (Graphing Dependency)
getDeps project = analyzeGopkgLock project <||> analyzeGopkgToml project

analyzeGopkgLock :: (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m) => GodepProject -> m (Graphing Dependency)
analyzeGopkgLock project = Diag.fromMaybeText "No Gopkg.lock present in the project" (godepLock project) >>= GopkgLock.analyze'

analyzeGopkgToml :: (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m) => GodepProject -> m (Graphing Dependency)
analyzeGopkgToml project = Diag.fromMaybeText "No Gopkg.toml present in the project" (godepToml project) >>= GopkgToml.analyze'
