{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Strategy.Conda.CondaList (
  analyze,
  buildGraph,
  CondaListDep (..),
) where

import Control.Effect.Diagnostics (Diagnostics, Has)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import DepTypes (
  DepType (CondaType),
  Dependency (
    Dependency,
    dependencyEnvironments,
    dependencyLocations,
    dependencyName,
    dependencyTags,
    dependencyType,
    dependencyVersion
  ),
  VerConstraint (CEq),
 )
import Effect.Exec (
  AllowErr (Never),
  Command (Command, cmdAllowErr, cmdArgs, cmdName),
  Exec,
  execJson,
 )
import Graphing (Graphing, fromList)
import Path (Abs, Dir, Path)

-- conda list --json will output dependency data in JSON format
condaListCmd :: Command
condaListCmd =
  Command
    { cmdName = "conda"
    , cmdArgs = ["list", "--json"]
    , cmdAllowErr = Never
    }

buildGraph :: [CondaListDep] -> Graphing Dependency
buildGraph deps = Graphing.fromList (map toDependency deps)
  where
    toDependency :: CondaListDep -> Dependency
    toDependency CondaListDep{..} =
      Dependency
        { dependencyType = CondaType
        , dependencyName = listName
        , dependencyVersion = CEq <$> listVersion
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

analyze ::
  ( Has Exec sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m (Graphing Dependency)
analyze dir = do
  buildGraph <$> execJson @[CondaListDep] dir condaListCmd

data CondaListDep = CondaListDep
  { listName :: Text
  , listVersion :: Maybe Text
  , listBuild :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON CondaListDep where
  parseJSON = withObject "CondaListOutput" $ \obj ->
    CondaListDep <$> obj .: "name"
      <*> obj .:? "version"
      <*> obj .:? "build_string"
