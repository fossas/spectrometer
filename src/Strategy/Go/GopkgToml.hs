module Strategy.Go.GopkgToml
  ( discover

  , Gopkg(..)
  , PkgConstraint(..)

  , analyze
  , buildGraph
  )
  where

import Prologue hiding ((.=), empty)

import qualified Data.Map.Strict as M
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Toml (TomlCodec, (.=))
import qualified Toml

import DepTypes
import Diagnostics
import Discovery.Walk
import Effect.Exec
import Effect.LabeledGrapher
import Effect.ReadFS
import Graphing (Graphing)
import Strategy.Go.Transitive (fillInTransitive)
import Strategy.Go.Types
import Types

discover :: Discover
discover = Discover
  { discoverName = "gopkgtoml"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, ReadFS, Exec, Output ProjectClosure] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  case find (\f -> fileName f == "Gopkg.toml") files of
    Nothing -> pure ()
    Just file -> do
      res <- runError @ReadFSErr (analyze file)
      traverse_ output res

  walkContinue

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
  } deriving (Eq, Ord, Show, Generic)

data PkgConstraint = PkgConstraint
  { constraintName     :: Text
  , constraintSource   :: Maybe Text
  , constraintVersion  :: Maybe Text
  , constraintBranch   :: Maybe Text
  , constraintRevision :: Maybe Text
  }
  deriving (Eq, Ord, Show, Generic)

analyze :: Members '[ReadFS, Exec, Error ReadFSErr] r => Path Rel File -> Sem r ProjectClosure
analyze file = fmap (mkProjectClosure file) . graphingGolang $ do
  contents <- readContentsText file
  case Toml.decode gopkgCodec contents of
    Left err -> throw (FileParseError (fromRelFile file) (Toml.prettyException err))
    Right gopkg -> do
      buildGraph gopkg

      -- TODO: diagnostics?
      _ <- runError @ExecErr (fillInTransitive (parent file))
      pure ()

mkProjectClosure :: Path Rel File -> Graphing Dependency -> ProjectClosure
mkProjectClosure file graph = ProjectClosure
  { closureStrategyGroup = GolangGroup
  , closureStrategyName  = "golang-gopkgtoml"
  , closureModuleDir     = parent file
  , closureDependencies  = dependencies
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = graph
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = NotComplete
    }

buildGraph :: Member (LabeledGrapher GolangPackage) r => Gopkg -> Sem r ()
buildGraph = void . M.traverseWithKey go . resolve
  where
  go :: Member (LabeledGrapher GolangPackage) r => Text -> PkgConstraint -> Sem r ()
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
