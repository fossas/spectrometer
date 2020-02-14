
module App.Scan.Project
  ( Project(..)
  , ProjectStrategy(..)

  , mkProjects
  ) where

import Prologue

import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.List (findIndex)

import App.Scan.Graph (Graph)
import App.Scan.GraphMangler (graphingToGraph)
import Types

data Project = Project
  { projectPath       :: Path Rel Dir
  , projectStrategies :: S.Seq ProjectStrategy
  }
  deriving (Eq, Ord, Show, Generic)

data ProjectStrategy = ProjectStrategy
  { projStrategyName     :: Text
  , projStrategyGraph    :: Graph
  , projStrategyOptimal  :: Optimal
  , projStrategyComplete :: Complete
  } deriving (Eq, Ord, Show, Generic)

type StrategyName = Text
type StrategyGroupName = Text

mkProjects :: [StrategyGroup] -> S.Seq ProjectClosure -> [Project]
mkProjects groups = toProjects . grouping
  where
  toProjects :: Map (StrategyGroupName, Path Rel Dir) (S.Seq ProjectClosure) -> [Project]
  toProjects = fmap toProject . M.toList

  toProject :: ((StrategyGroupName, Path Rel Dir), S.Seq ProjectClosure) -> Project
  toProject ((_, dir), closures) = Project
    { projectPath = dir
    , projectStrategies = fmap toProjectStrategy (S.sortOn (ixInGroup . closureStrategyName) closures)
    }

  toProjectStrategy :: ProjectClosure -> ProjectStrategy
  toProjectStrategy ProjectClosure{..} =
    ProjectStrategy { projStrategyName = closureStrategyName
                    , projStrategyGraph = graphingToGraph (dependenciesGraph closureDependencies)
                    , projStrategyOptimal = dependenciesOptimal closureDependencies
                    , projStrategyComplete = dependenciesComplete closureDependencies
                    }

  grouping :: S.Seq ProjectClosure -> Map (StrategyGroupName, Path Rel Dir) (S.Seq ProjectClosure)
  grouping closures = M.fromListWith (<>) $ toList $ do
    closure <- closures
    let groupName = closureToGroup closure
        moduleDir = closureModuleDir closure

    pure ((groupName, moduleDir), S.singleton closure)

  closureToGroup :: ProjectClosure -> StrategyGroupName
  closureToGroup ProjectClosure{closureStrategyName} =
    -- use the strategy name as a group name if a group doesn't exist
    fromMaybe closureStrategyName (M.lookup closureStrategyName groupsByStrategy)

  ixInGroup :: StrategyName -> Int
  ixInGroup stratName = fromMaybe 0 $ do -- Maybe monad
    groupName <- M.lookup stratName groupsByStrategy
    group     <- M.lookup groupName groupsByName

    findIndex (\strat -> strat == stratName)
              (groupStrategies group)

  groupsByName :: Map StrategyGroupName StrategyGroup
  groupsByName = M.fromList [(groupName group, group) | group <- groups]

  groupsByStrategy :: Map StrategyName StrategyGroupName
  groupsByStrategy = M.fromList
    [(stratName, groupName) | StrategyGroup groupName strategies <- groups
                            , stratName <- strategies
                            ]

instance ToJSON Project where
  toJSON Project{..} = object
    [ "path"       .= projectPath
    , "strategies" .= projectStrategies
    ]

instance ToJSON ProjectStrategy where
  toJSON ProjectStrategy{..} = object
    [ "name"     .= projStrategyName
    , "graph"    .= projStrategyGraph
    , "optimal"  .= projStrategyOptimal
    , "complete" .= projStrategyComplete
    ]
