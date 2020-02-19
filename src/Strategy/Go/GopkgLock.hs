module Strategy.Go.GopkgLock
  ( discover
  , analyze

  , Project(..)

  , buildGraph
  )
  where

import Prologue hiding ((.=))

import Control.Carrier.Error.Either
import Control.Effect.Output
import DepTypes
import Diagnostics
import Discovery.Walk
import Effect.Exec
import Effect.LabeledGrapher
import Effect.ReadFS
import Graphing (Graphing)
import Strategy.Go.Transitive (fillInTransitive)
import Strategy.Go.Types
import qualified Toml
import Toml (TomlCodec, (.=))
import Types

discover :: Discover
discover = Discover
  { discoverName = "gopkglock"
  , discoverFunc = discover'
  }

discover' ::
  ( Has ReadFS sig m
  , Has Exec sig m
  , Has (Output ProjectClosure) sig m
  , MonadIO m
  , Effect sig
  )
  => Path Abs Dir -> m ()
discover' = walk $ \_ _ files -> do
  case find (\f -> fileName f == "Gopkg.lock") files of
    Nothing -> pure ()
    Just file -> do
      res <- runError @ReadFSErr (analyze file)
      traverse_ output res

  walkContinue

golockCodec :: TomlCodec GoLock
golockCodec = GoLock
  <$> Toml.list projectCodec "projects" .= lockProjects

projectCodec :: TomlCodec Project
projectCodec = Project
  <$> Toml.text "name" .= projectName
  <*> Toml.dioptional (Toml.text "source") .= projectSource
  <*> Toml.text "revision" .= projectRevision

newtype GoLock = GoLock
  { lockProjects :: [Project]
  } deriving (Eq, Ord, Show, Generic)

data Project = Project
  { projectName     :: Text
  , projectSource   :: Maybe Text
  , projectRevision :: Text
  } deriving (Eq, Ord, Show, Generic)

analyze ::
  ( Has ReadFS sig m
  , Has (Error ReadFSErr) sig m
  , Has Exec sig m
  , Effect sig
  )
  => Path Rel File -> m ProjectClosure
analyze file = fmap (mkProjectClosure file) . graphingGolang $ do
  contents <- readContentsText file
  case Toml.decode golockCodec contents of
    Left err -> throwError (FileParseError (fromRelFile file) (Toml.prettyException err))
    Right golock -> do
      buildGraph (lockProjects golock)

      -- TODO: diagnostics?
      _ <- runError @ExecErr (fillInTransitive (parent file))
      pure ()

mkProjectClosure :: Path Rel File -> Graphing Dependency -> ProjectClosure
mkProjectClosure file graph = ProjectClosure
  { closureStrategyGroup = GolangGroup
  , closureStrategyName  = "golang-gopkglock"
  , closureModuleDir     = parent file
  , closureDependencies  = dependencies
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = graph
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = NotComplete
    }

buildGraph :: Has (LabeledGrapher GolangPackage) sig m => [Project] -> m ()
buildGraph = void . traverse_ go
  where
  go :: Has (LabeledGrapher GolangPackage) sig m => Project -> m ()
  go Project{..} = do
    let pkg = mkGolangPackage projectName

    direct pkg
    label pkg (mkGolangVersion projectRevision)

    -- label location when it exists
    traverse_ (label pkg . GolangLabelLocation) projectSource
