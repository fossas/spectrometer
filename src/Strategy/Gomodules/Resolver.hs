{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Strategy.Gomodules.Resolver
  ( findProjects,
    GlobalClosure (..),
    GomodulesProject (..),
  )
where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AM
import Control.Carrier.State.Strict
import Control.Effect.Diagnostics
import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Discovery.Walk
import Effect.Grapher
import Effect.ReadFS
import Graphing (graphingAdjacent)
import Path
import qualified Strategy.Go.Gomod as Gomod

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, MonadIO m) => Path Abs Dir -> m [GomodulesProject]
findProjects basedir = do
  gomodFiles <- findGomodFiles basedir
  (fileGraph, (loadResults, ())) <- runGrapher @(Path Abs File) . runState @LoadResults M.empty $ traverse_ recursiveLoadGomod gomodFiles
  pure $ buildProjectClosures (graphingAdjacent fileGraph) (M.mapMaybe id loadResults)

buildProjectClosures :: AM.AdjacencyMap (Path Abs File) -> Map (Path Abs File) Gomod.Gomod -> [GomodulesProject]
buildProjectClosures fileGraph gomods = closures
  where
    closures :: [GomodulesProject]
    closures = mapMaybe (\gomodPath -> toClosure gomodPath <$> M.lookup gomodPath gomods) graphRoots

    toClosure :: Path Abs File -> Gomod.Gomod -> GomodulesProject
    toClosure gomodPath gomod = GomodulesProject gomodPath gomod reachableGraph reachableGomodMap
      where
        reachableGraph = AM.induce (`S.member` reachableGomods) fileGraph
        reachableGomodMap = M.filterWithKey (\k _ -> S.member k reachableGomods) gomods
        reachableGomods = S.fromList $ AM.reachable gomodPath fileGraph

    graphRoots :: [Path Abs File]
    graphRoots = sourceVertices fileGraph

sourceVertices :: Ord a => AM.AdjacencyMap a -> [a]
sourceVertices graph = [v | v <- AM.vertexList graph, S.null (AM.preSet v graph)]

recursiveLoadGomod :: (Has ReadFS sig m, Has (Grapher (Path Abs File)) sig m, Has (State LoadResults) sig m, Has Diagnostics sig m) => Path Abs File -> m ()
recursiveLoadGomod file = do
  results <- get @LoadResults

  case M.lookup file results of
    -- don't re-inspect this same path
    Just _ -> pure ()
    Nothing -> do
      (res :: Maybe Gomod.Gomod) <- recover (readContentsParser Gomod.gomodParser file)
      modify @LoadResults (M.insert file res)
      traverse_ (addEdges file) res

addEdges :: (Has ReadFS sig m, Has Diagnostics sig m, Has (State LoadResults) sig m, Has (Grapher (Path Abs File)) sig m) => Path Abs File -> Gomod.Gomod -> m ()
addEdges gomodPath gomod = do
  resolvedPaths <- traverse (resolveDir (parent gomodPath)) pathTexts
  let resolvedGomods = map (\dir -> dir </> [relfile|go.mod|]) resolvedPaths
  traverse_ (edge gomodPath) resolvedGomods
  traverse_ recursiveLoadGomod resolvedGomods
  where
    pathTexts = M.elems (Gomod.modLocalReplaces gomod)

type LoadResults = Map (Path Abs File) (Maybe Gomod.Gomod)

findGomodFiles :: MonadIO m => Path Abs Dir -> m [Path Abs File]
findGomodFiles = walk' $ \_ _ files ->
  case findFileNamed "go.mod" files of
    Nothing -> pure ([], WalkSkipSome ["vendor"])
    Just gomod -> pure ([gomod], WalkSkipSome ["vendor"])

data GlobalClosure = GlobalClosure
  { globalGraph :: AM.AdjacencyMap (Path Abs File),
    globalGomods :: Map (Path Abs File) Gomod.Gomod
  }

data GomodulesProject = GomodulesProject
  { closureRootGomodPath :: Path Abs File,
    closureRootGomod :: Gomod.Gomod,
    closureGraph :: AM.AdjacencyMap (Path Abs File),
    closureGomods :: Map (Path Abs File) Gomod.Gomod
  } deriving Show
