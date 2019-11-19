{-# language TemplateHaskell #-}

module Strategy.Go.Types
  ( GolangPackage(..)
  , GolangLabel(..)
  , Graphing(..)
  , directg
  , edgeg
  , labelg

  , graphingToGraphBuilder
  , fixVersion
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

type family PkgLabel (pkg :: Type) :: Type

type instance PkgLabel GolangPackage = GolangLabel

-- TODO: unexpose GolangVersion and use smart constructure that applies fixVersion first?
data GolangLabel = GolangVersion Text | GolangLocation Text
  deriving (Eq, Ord, Show, Generic)

data Graphing ty m a where
  -- TODO: direct dependencies
  Directg :: ty -> Graphing ty m ()
  Edgeg :: ty -> ty -> Graphing ty m ()
  Labelg :: ty -> (PkgLabel ty) -> Graphing ty m ()

makeSem ''Graphing

-- TODO: document semantics around reachable deps -- i.e., anything we want to include needs to be reachable from a direct dep
-- TODO: eliminate GraphBuilder here
graphingToGraphBuilder :: forall ty lbl r a . (lbl ~ PkgLabel ty, Ord ty, Ord lbl, Member (GraphBuilder) r) => (ty -> Set lbl -> G.Dependency) -> Sem (Graphing ty ': r) a -> Sem r ()
graphingToGraphBuilder toDependency act = do
  (amap, lbls, direct, _) <- runGraphing act

  let amap' = gmap mkDependency amap
      direct' = fmap mkDependency (S.toList direct)

      mkDependency a = toDependency a (fromMaybe S.empty (M.lookup a lbls))

      nodes = dfs direct' amap'


  refs <- M.fromList <$> traverse addingNode nodes

  traverse_ (visitNode refs amap') nodes

  -- add direct
  traverse_ (\dep -> traverse_ addDirect (M.lookup dep refs)) direct'

  where

  addingNode :: G.Dependency -> Sem r (G.Dependency, G.DepRef)
  addingNode k = do
    ref <- addNode k
    pure (k, ref)

  visitNode :: Map G.Dependency G.DepRef -> AdjacencyMap G.Dependency -> G.Dependency -> Sem r ()
  visitNode refs amap node = traverse_ (f refs node) (S.toList $ postSet node amap)

  f :: Map G.Dependency G.DepRef -> G.Dependency -> G.Dependency -> Sem r ()
  f refs parent child = do
    let edgeRefs = do
          parentRef <- M.lookup parent refs
          childRef <- M.lookup child refs
          pure (parentRef, childRef)

    traverse_ (\(parentRef, childRef) -> addEdge parentRef childRef) edgeRefs

    pure ()

runGraphing :: forall ty lbl r a. (lbl ~ PkgLabel ty, Ord ty, Ord lbl) => Sem (Graphing ty ': r) a -> Sem r (AdjacencyMap ty, Map ty (Set lbl), Set ty, a)
runGraphing = fmap (\(amap, (lbls, (direct, a))) -> (amap, lbls, direct, a))
            . runState @(AdjacencyMap ty) empty
            . runState @(Map ty (Set lbl)) M.empty
            . runState @(Set ty) S.empty
            . reinterpret3 (\case
  Directg v -> modify (S.insert v)
  Edgeg v1 v2 -> modify (overlay (edge v1 v2))
  Labelg v lbl -> modify (M.insertWith (<>) v (S.singleton lbl)))

-- TODO: do this everywhere we touch versions
-- TODO: unvendor
fixVersion :: Text -> Text
fixVersion = last . T.splitOn "-" . T.replace "+incompatible" ""
