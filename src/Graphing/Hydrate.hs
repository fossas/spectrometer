{-# LANGUAGE FunctionalDependencies #-}

module Graphing.Hydrate (
  Hydrateable (..),
  hydrate,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AMA
import Data.Containers.ListUtils (nubOrd)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Graphing (Graphing)
import Graphing qualified

-- | Given some 'Graphing a' with an instance of @Hydrateable a b@, update the
-- nodes such that all items (which are type @b@) of a node are copied down to
-- all of its successor nodes.
hydrate :: forall a b. Hydrateable a b => Graphing a -> Graphing a
hydrate gr = Graphing.gmap doPromotion gr
  where
    adjMap = Graphing.toAdjacencyMap gr
    -- Get all current nodes which contain a specified subitem
    topVia subItem = AM.vertexList $ AM.induce (elem subItem . extractList) adjMap
    -- Get all nodes reachable from a list of nodes
    allFrom item = Set.fromList $ concatMap (`AMA.reachable` adjMap) (topVia item)

    -- Dedup'd sub-items from all vertices in the AdjMap
    allSubItems :: [b]
    allSubItems = nubOrd $ foldMap extractList $ AM.vertexList adjMap

    -- Final map of all promotions, with de-duplicated keys
    promotionMap :: Map a [b]
    promotionMap = Map.map nubOrd $ foldr extract Map.empty allSubItems

    -- Update the map for each sub item
    extract :: b -> Map a [b] -> Map a [b]
    extract item mapping = foldr (addItemToMap item) mapping $ allFrom item

    addItemToMap :: b -> a -> Map a [b] -> Map a [b]
    addItemToMap v k mapping =
      if k `Map.member` mapping
        then -- Add item to list of keys
          Map.insert k (v : (mapping Map.! k)) mapping
        else -- Add singleton list
          Map.insert k [v] mapping

    doPromotion :: a -> a
    doPromotion item = foldr update item $ Map.findWithDefault [] item promotionMap

class (Ord a, Ord b) => Hydrateable a b | a -> b where
  extractList :: a -> [b]
  update :: b -> a -> a
