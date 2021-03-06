{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Strategy.Go.GlideLock
  ( analyze'

  , GlideLockfile(..)
  , GlideDep(..)

  , buildGraph
  )
  where

import Control.Effect.Diagnostics
import Data.Aeson
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import DepTypes
import Effect.ReadFS
import Graphing (Graphing)
import qualified Graphing
import Path

analyze' :: (Has ReadFS sig m , Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze' file = buildGraph <$> readContentsYaml @GlideLockfile file

buildGraph :: GlideLockfile -> Graphing Dependency
buildGraph lockfile = Graphing.fromList (map toDependency direct)
  where
  direct = imports lockfile
  toDependency GlideDep{..} =
    Dependency { dependencyType = GoType
               , dependencyName = depName
               , dependencyVersion = Just (CEq $ T.pack (show depVersion))
               , dependencyLocations = []
               , dependencyEnvironments = []
               , dependencyTags = M.empty
               }

data GlideLockfile = GlideLockfile
  { hash    :: Text
  , updated :: Text
  , imports :: [GlideDep]
  } deriving (Eq, Ord, Show)

data GlideDep = GlideDep
  { depName    :: Text
  , depVersion :: Integer
  , depRepo    :: Maybe Text
  } deriving (Eq, Ord, Show)

instance FromJSON GlideLockfile where
  parseJSON = withObject "GlideLockfile" $ \obj ->
    GlideLockfile <$> obj .: "hash"
                  <*> obj .: "updated"
                  <*> obj .: "imports"

instance FromJSON GlideDep where
  parseJSON = withObject "GlideDep" $ \obj ->
    GlideDep <$> obj .:  "name"
               <*> obj .: "version"
               <*> obj .:? "repo"
