{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Strategy.Go.GopkgToml
  ( discover

  , Gopkg(..)
  , PkgConstraint(..)

  , analyze
  , analyze'
  , buildGraph

  , gopkgCodec
  )
  where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics
import Data.Foldable (find, traverse_)
import Data.Functor (void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import DepTypes
import Discovery.Walk
import Effect.Exec
import Effect.Grapher
import Effect.ReadFS
import Graphing (Graphing)
import Path
import Strategy.Go.Transitive (fillInTransitive)
import Strategy.Go.Types
import Toml (TomlCodec, (.=))
import qualified Toml
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> fileName f == "Gopkg.toml") files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "golang-gopkgtoml" GolangGroup $ analyze file

  pure $ WalkSkipSome ["vendor"]

gopkgCodec :: TomlCodec Gopkg
gopkgCodec = Gopkg
  <$> Toml.list constraintCodec "constraint" .= pkgConstraints
  <*> Toml.list constraintCodec "override" .= pkgOverrides

constraintCodec :: TomlCodec PkgConstraint
constraintCodec = PkgConstraint
  <$> Toml.text "name" .= constraintName
  <*> Toml.dioptional (Toml.text "source") .= constraintSource
  <*> Toml.dioptional (Toml.text "version") .= constraintVersion
  <*> Toml.dioptional (Toml.text "branch") .= constraintBranch
  <*> Toml.dioptional (Toml.text "revision") .= constraintRevision

data Gopkg = Gopkg
  { pkgConstraints :: [PkgConstraint]
  , pkgOverrides   :: [PkgConstraint]
  } deriving (Eq, Ord, Show)

data PkgConstraint = PkgConstraint
  { constraintName     :: Text
  , constraintSource   :: Maybe Text
  , constraintVersion  :: Maybe Text
  , constraintBranch   :: Maybe Text
  , constraintRevision :: Maybe Text
  }
  deriving (Eq, Ord, Show)

analyze' ::
  ( Has ReadFS sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  )
  => Path Abs File -> m (Graphing Dependency)
analyze' file = graphingGolang $ do
  gopkg <- readContentsToml gopkgCodec file
  buildGraph gopkg

  _ <- recover (fillInTransitive (parent file))
  pure ()

analyze ::
  ( Has ReadFS sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  )
  => Path Abs File -> m ProjectClosureBody
analyze file = mkProjectClosure file <$> analyze' file

mkProjectClosure :: Path Abs File -> Graphing Dependency -> ProjectClosureBody
mkProjectClosure file graph = ProjectClosureBody
  { bodyModuleDir     = parent file
  , bodyDependencies  = dependencies
  , bodyLicenses      = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = graph
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = NotComplete
    }

buildGraph :: Has GolangGrapher sig m => Gopkg -> m ()
buildGraph = void . M.traverseWithKey go . resolve
  where
  go :: Has GolangGrapher sig m => Text -> PkgConstraint -> m ()
  go name PkgConstraint{..} = do
    let pkg = mkGolangPackage name

    direct pkg

    -- label version when it exists
    traverse_ (label pkg . mkGolangVersion)
              (constraintVersion <|> constraintBranch <|> constraintRevision)

    -- label location when it exists
    traverse_ (label pkg . GolangLabelLocation) constraintSource

-- TODO: handling version constraints
resolve :: Gopkg -> Map Text PkgConstraint -- Map Package (Maybe Version)
resolve gopkg = overridden
  where
  overridden = foldr inserting constraints (pkgOverrides gopkg)
  constraints = foldr inserting M.empty (pkgConstraints gopkg)

  inserting :: PkgConstraint -> Map Text PkgConstraint -> Map Text PkgConstraint
  inserting constraint = M.insert (constraintName constraint) constraint
