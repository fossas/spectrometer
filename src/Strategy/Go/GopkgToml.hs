module Strategy.Go.GopkgToml
  ( discover
  , strategy
  , analyze
  , configure

  , Gopkg(..)
  , PkgConstraint(..)

  , buildGraph
  )
  where

import Prologue hiding ((.=))

import qualified Data.Map.Strict as M
import           Data.Maybe (maybeToList)
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Toml (TomlCodec, (.=))
import qualified Toml

import           Diagnostics
import           Discovery.Walk
import           Effect.Exec
import           Effect.ReadFS
import           Effect.GraphBuilder
import qualified Graph as G
import           Strategy.Go.Transitive (fillInTransitive)
import           Types

discover :: Discover
discover = Discover
  { discoverName = "gopkgtoml"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files ->
  case find (\f -> fileName f == "Gopkg.toml") files of
    Nothing -> walkContinue
    Just file  -> do
      output (configure file)
      walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "golang-gopkgtoml"
  , strategyAnalyze = analyze
  , strategyModule = parent . targetFile
  , strategyOptimal = NotOptimal
  , strategyComplete = NotComplete
  }

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

analyze :: Members '[ReadFS, Exec, Error ReadFSErr, Error ExecErr] r => BasicFileOpts -> Sem r G.Graph
analyze BasicFileOpts{..} = do
  contents <- readContentsText targetFile
  case Toml.decode gopkgCodec contents of
    Left err -> throw (FileParseError (fromRelFile targetFile) (Toml.prettyException err))
    Right gopkg -> do
      let (incompleteGraph, mapping) = buildGraph gopkg

      graph <- fillInTransitive mapping (parent targetFile) incompleteGraph `catch` (\(_ :: ExecErr) -> pure incompleteGraph)

      pure graph

type PackageName = Text

buildGraph :: Gopkg -> (G.Graph, Map PackageName G.DepRef)
buildGraph gopkg = run . runGraphBuilder G.empty $ do
  --unfold direct (const []) toDependency
  let constraints :: Map Text PkgConstraint
      constraints = resolve gopkg

  (assocs, _) <- runOutputList $ M.traverseWithKey addPkgConstraint constraints

  let mapping :: Map PackageName G.DepRef
      mapping = M.fromList assocs

  pure mapping

  where

  addPkgConstraint :: Members '[GraphBuilder, Output (PackageName, G.DepRef)] r => Text -> PkgConstraint -> Sem r ()
  addPkgConstraint name constraint = do
    ref <- addNode (toDependency name constraint)
    addDirect ref
    output (name, ref)

  --direct = M.toList (resolve gopkg)

  toDependency name PkgConstraint{..} =
    G.Dependency { dependencyType = G.GoType
                 , dependencyName = name
                 , dependencyVersion = G.CEq <$> (constraintVersion <|> constraintBranch <|> constraintRevision)
                 , dependencyLocations = maybeToList constraintSource
                 , dependencyTags = M.empty
                 }

-- TODO: handling version constraints
resolve :: Gopkg -> Map Text PkgConstraint -- Map Package (Maybe Version)
resolve gopkg = overridden
  where
  overridden = foldr inserting constraints (pkgOverrides gopkg)
  constraints = foldr inserting M.empty (pkgConstraints gopkg)

  inserting :: PkgConstraint -> Map Text PkgConstraint -> Map Text PkgConstraint
  inserting constraint = M.insert (constraintName constraint) constraint

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts
