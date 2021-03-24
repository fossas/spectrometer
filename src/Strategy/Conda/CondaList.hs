{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Strategy.Conda.CondaList (
  analyze',
  buildGraph,
  CondaListDep(..)
) where

import Effect.Exec
import qualified Data.Map.Strict as M
import Control.Carrier.Diagnostics hiding (fromMaybe)
import Graphing (Graphing, fromList)
import Data.Aeson
import Data.Text (Text)
import Path
import Types

-- conda list --json will output dependency data in JSON format
condaListCmd :: Command
condaListCmd = Command
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
      {
        dependencyType = CondaType,
        dependencyName = listName,
        dependencyVersion = CEq <$> listVersion, -- todo - make this real
        dependencyLocations = [],
        dependencyEnvironments = [],
        dependencyTags = M.empty
      }

analyze' :: 
  ( Has Exec sig m
  , Has Diagnostics sig m
  )
  => Path Abs Dir -> m (Graphing Dependency)
analyze' dir = do
  buildGraph <$> execJson @[CondaListDep] dir condaListCmd


data CondaListDep = 
    CondaListDep
    { listName :: Text
     ,listVersion :: Maybe Text
     ,listBuild :: Maybe Text
    } deriving (Eq, Ord, Show)

instance FromJSON CondaListDep where
  parseJSON = withObject "CondaListOutput" $ \obj ->
    CondaListDep <$> obj .: "name"
              <*> obj .:? "version"
              <*> obj .:? "build_string"
