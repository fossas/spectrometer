{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Strategy.Conda.EnvironmentYml (
  analyze',
  buildGraph,
  EnvironmentYmlFile(..)
) where

import Effect.Exec
import Effect.ReadFS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Carrier.Diagnostics hiding (fromMaybe)
import Graphing (Graphing, fromList)
import Data.Aeson
import Data.Text (Text)
import Path
import Types


buildGraph :: EnvironmentYmlFile -> Graphing Dependency
buildGraph envYmlFile = Graphing.fromList (map toDependency allDeps)
  where
    allDeps = map getCondaDepFromText (dependencies envYmlFile)
    toDependency :: CondaDep -> Dependency
    toDependency CondaDep{..} =
       Dependency 
       {
        dependencyType = CondaType,
        dependencyName = depName,
        dependencyVersion = CEq <$> depVersion, -- todo - make this real
        dependencyLocations = [],
        dependencyEnvironments = [],
        dependencyTags = M.empty
      }

analyze' :: 
 ( Has ReadFS sig m
 , Has Diagnostics sig m
 )
  => Path Abs File -> m (Graphing Dependency)
analyze' envYml = do
  buildGraph <$> readContentsYaml @EnvironmentYmlFile envYml


-- an example Text: "biopython=1.78=py38haf1e3a3_0"
-- '=' is a delimeter for <name>=<version>=<build>
-- where <version> and <build> are optional
getCondaDepFromText :: Text -> CondaDep
getCondaDepFromText rcd =
  CondaDep
  { depName = name
  , depVersion = version
  , depBuild = build
  , depFullVersion = fullVersion
  }
  
  where
    depSplit = T.split (== '=') rcd

    name = head depSplit
    version = depSplit !? 1 -- TODO: this may contain constraints that we need to parse
    build = depSplit !? 2
    fullVersion = getFullVersion version build

    -- TODO: move to reusable file
    (!?) :: [a] -> Int -> Maybe a
    xs !? ix
      | length xs <= ix = Nothing
      | otherwise = Just (xs !! ix)

    getFullVersion :: Maybe Text -> Maybe Text -> Maybe Text
    getFullVersion a b = do
      aVal <- a
      let bVal = case b of
            Just x -> "=" <> x
            Nothing -> ""
      pure (aVal <> bVal)


-- TODO: do something with channels?
data EnvironmentYmlFile =
    EnvironmentYmlFile
    { name :: Text
    , dependencies :: [Text]
    } deriving (Eq, Ord, Show)

instance FromJSON EnvironmentYmlFile where
  parseJSON = withObject "EnvironmentYmlFile" $ \obj ->
    EnvironmentYmlFile <$> obj .: "name"
                       <*> obj .: "dependencies"

data CondaDep = CondaDep
  { depName :: Text
  , depVersion :: Maybe Text  
  , depBuild :: Maybe Text
  , depFullVersion :: Maybe Text
  } deriving (Eq, Ord, Show)
