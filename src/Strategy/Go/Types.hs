{-# language TemplateHaskell #-}

module Strategy.Go.Types
  ( GolangPackage -- don't export GolangPackage; export the smart constructor instead
  , mkGolangPackage
  , GolangLabel(GolangLabelLocation) -- don't export GolangLabelVersion; export the smart constructor instead
  , mkGolangVersion
  , Graphing(..)
  , directg
  , edgeg
  , labelg

  , graphingToGraph
  , golangPackageToDependency
  )
  where

import Prologue hiding (empty, parent)

import Data.Kind
import Effect.GraphBuilder
import Polysemy
import Polysemy.State

import           Algebra.Graph.AdjacencyMap
import           Algebra.Graph.ToGraph (dfs)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Graph as G

-- TODO: docs around Eq/Ord semantics here wrt graph building
newtype GolangPackage = GolangPackage { goImportPath :: Text } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for @GolangPackage@. Applies 'unvendor' to the vlaue
mkGolangPackage :: Text -> GolangPackage
mkGolangPackage = GolangPackage . unvendor

type family PkgLabel (pkg :: Type) :: Type

type instance PkgLabel GolangPackage = GolangLabel

golangPackageToDependency :: GolangPackage -> Set GolangLabel -> G.Dependency
golangPackageToDependency pkg = foldr applyLabel start
  where

  start :: G.Dependency
  start = G.Dependency
    { dependencyType = G.GoType
    , dependencyName = goImportPath pkg
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyTags = M.empty
    }

  applyLabel :: GolangLabel -> G.Dependency -> G.Dependency
  applyLabel (GolangLabelVersion ver) dep = dep { G.dependencyVersion = Just (G.CEq ver) }
  applyLabel (GolangLabelLocation loc) dep = dep { G.dependencyLocations = loc : (G.dependencyLocations dep) }

data GolangLabel = GolangLabelVersion Text | GolangLabelLocation Text
  deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for GolangLabelVersion. Applies 'fixVersion' to the value
mkGolangVersion :: Text -> GolangLabel
mkGolangVersion = GolangLabelVersion . fixVersion

-- replace "v0.0.0-20191212000000-abcdef+incompatible" with "abcdef"
fixVersion :: Text -> Text
fixVersion = last . T.splitOn "-" . T.replace "+incompatible" ""

-- replace "github.com/A/B/vendor/github.com/X/Y" with "github.com/X/Y"
unvendor :: Text -> Text
unvendor = last . T.splitOn "/vendor/"

data Graphing ty m a where
  -- TODO: direct dependencies
  Directg :: ty -> Graphing ty m ()
  Edgeg :: ty -> ty -> Graphing ty m ()
  Labelg :: ty -> (PkgLabel ty) -> Graphing ty m ()

makeSem ''Graphing

-- TODO: document semantics around reachable deps -- i.e., anything we want to include needs to be reachable from a direct dep
graphingToGraph :: forall ty lbl r a . (lbl ~ PkgLabel ty, Ord ty, Ord lbl) => (ty -> Set lbl -> G.Dependency) -> Sem (Graphing ty ': r) a -> Sem r G.Graph
graphingToGraph toDependency act = evalGraphBuilder G.empty $ do
  (amap, lbls, direct, _) <- runGraphingPure (raiseUnder act)

  let depAmap = gmap mkDependency amap
      depDirect = fmap mkDependency (S.toList direct)

      mkDependency :: ty -> G.Dependency
      mkDependency a = toDependency a (fromMaybe S.empty (M.lookup a lbls))

      nodes = dfs depDirect depAmap

  refs <- M.fromList <$> traverse addingNode nodes

  traverse_ (visitNode refs depAmap) nodes

  traverse_ (\dep -> traverse_ addDirect (M.lookup dep refs)) depDirect

  where

  -- add a node with GraphBuilder
  addingNode :: Member GraphBuilder r' => G.Dependency -> Sem r' (G.Dependency, G.DepRef)
  addingNode k = do
    ref <- addNode k
    pure (k, ref)

  -- visit a node, adding edges between it and all of its dependencies
  visitNode :: Member GraphBuilder r' => Map G.Dependency G.DepRef -> AdjacencyMap G.Dependency -> G.Dependency -> Sem r' ()
  visitNode refs amap node = traverse_ (visitEdge refs node) (S.toList $ postSet node amap)

  -- visit an edge by adding it to the graph
  visitEdge :: Member GraphBuilder r' => Map G.Dependency G.DepRef -> G.Dependency -> G.Dependency -> Sem r' ()
  visitEdge refs parent child = do
    let edgeRefs = do
          parentRef <- M.lookup parent refs
          childRef <- M.lookup child refs
          pure (parentRef, childRef)

    traverse_ (\(parentRef, childRef) -> addEdge parentRef childRef) edgeRefs

runGraphingPure :: forall ty lbl r a. (lbl ~ PkgLabel ty, Ord ty, Ord lbl) => Sem (Graphing ty ': r) a -> Sem r (AdjacencyMap ty, Map ty (Set lbl), Set ty, a)
runGraphingPure = fmap (\(amap, (lbls, (direct, a))) -> (amap, lbls, direct, a))
                . runState @(AdjacencyMap ty) empty
                . runState @(Map ty (Set lbl)) M.empty
                . runState @(Set ty) S.empty
                . reinterpret3 (\case
  Directg v -> modify (S.insert v)
  Edgeg v1 v2 -> modify (overlay (edge v1 v2))
  Labelg v lbl -> modify (M.insertWith (<>) v (S.singleton lbl)))
