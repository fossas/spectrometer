{-# LANGUAGE RecordWildCards #-}

module Strategy.Go.GopkgLock (
  analyze',
  GoLock (..),
  Project (..),
  buildGraph,
  golockCodec,
) where

import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  recover,
 )
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Text (Text)
import DepTypes (Dependency)
import Effect.Exec (Exec)
import Effect.Grapher (direct, label)
import Effect.ReadFS (ReadFS, readContentsToml)
import Graphing (Graphing)
import Path (Abs, File, Path, parent)
import Strategy.Go.Transitive (fillInTransitive)
import Strategy.Go.Types (
  GolangGrapher,
  GolangLabel (GolangLabelLocation),
  graphingGolang,
  mkGolangPackage,
  mkGolangVersion,
 )
import Toml (TomlCodec, (.=))
import Toml qualified

golockCodec :: TomlCodec GoLock
golockCodec =
  GoLock
    <$> Toml.list projectCodec "projects" .= lockProjects

projectCodec :: TomlCodec Project
projectCodec =
  Project
    <$> Toml.text "name" .= projectName
    <*> Toml.dioptional (Toml.text "source") .= projectSource
    <*> Toml.text "revision" .= projectRevision

newtype GoLock = GoLock
  { lockProjects :: [Project]
  }
  deriving (Eq, Ord, Show)

data Project = Project
  { projectName :: Text
  , projectSource :: Maybe Text
  , projectRevision :: Text
  }
  deriving (Eq, Ord, Show)

analyze' ::
  ( Has ReadFS sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs File ->
  m (Graphing Dependency)
analyze' file = graphingGolang $ do
  golock <- readContentsToml golockCodec file
  context "Building dependency graph" $ buildGraph (lockProjects golock)
  _ <- recover (fillInTransitive (parent file))
  pure ()

buildGraph :: Has GolangGrapher sig m => [Project] -> m ()
buildGraph = void . traverse_ go
  where
    go :: Has GolangGrapher sig m => Project -> m ()
    go Project{..} = do
      let pkg = mkGolangPackage projectName

      direct pkg
      label pkg (mkGolangVersion projectRevision)

      -- label location when it exists
      traverse_ (label pkg . GolangLabelLocation) projectSource
