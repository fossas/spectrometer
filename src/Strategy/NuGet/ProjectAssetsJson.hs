module Strategy.NuGet.ProjectAssetsJson
  ( discover
  , buildGraph
  , analyze

  , ProjectAssetsJson(..)
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Maybe
import Polysemy
import Polysemy.Error
import Polysemy.Output

import Diagnostics
import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing, unfold)
import Types

discover :: Discover
discover = Discover
  { discoverName = "projectassetsjson"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, ReadFS, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  case find (\f -> fileName f == "project.assets.json") files of
    Nothing -> pure ()
    Just file -> do
      res <- runError @ReadFSErr (analyze file)
      traverse_ (output . dummyConfigure "nuget-projectassetsjson" NotOptimal NotComplete (parent file)) res

  walkContinue

data ProjectAssetsJson = ProjectAssetsJson
  { targets     :: M.Map Text (M.Map Text DependencyInfo)
  } deriving Show

instance FromJSON ProjectAssetsJson where
  parseJSON = withObject "ProjectAssetsJson" $ \obj ->
    ProjectAssetsJson <$> obj .: "targets"

data DependencyInfo = DependencyInfo
  { depType    :: Text
  , deepDeps   :: M.Map Text Text
  } deriving Show
   
instance FromJSON DependencyInfo where
  parseJSON = withObject "Dependency" $ \obj ->
    DependencyInfo <$> obj .: "type"
             <*> obj .:? "dependencies" .!= M.empty

analyze :: Members '[ReadFS, Error ReadFSErr] r => Path Rel File -> Sem r (Graphing Dependency)
analyze file = do
  project <- readContentsJson @ProjectAssetsJson file
  pure (buildGraph project)

data NuGetDep = NuGetDep
  { depName            :: Text
  , depVersion         :: Text
  , completeDepType    :: Text
  , completeDeepDeps   :: M.Map Text Text
  } deriving Show

buildGraph :: ProjectAssetsJson -> Graphing Dependency
buildGraph project = unfold direct deepList toDependency
    where
    direct :: [NuGetDep] 
    direct = concatMap (mapMaybe convertDep . M.toList) (M.elems (targets project))
                    
    convertDep :: (Text, DependencyInfo) -> Maybe NuGetDep
    convertDep (depString, dep) = case T.splitOn "/" depString of
                  [name, ver] -> Just $ NuGetDep name ver (depType dep) (deepDeps dep) 
                  _ -> Nothing

    deepList nugetDep = (\(x,y) -> NuGetDep x y "" M.empty) <$> (M.toList $ completeDeepDeps nugetDep)
    toDependency NuGetDep{..} =
      Dependency { dependencyType = NuGetType
               , dependencyName = depName
               , dependencyVersion = Just (CEq depVersion)
               , dependencyLocations = []
               , dependencyTags = M.empty
               }
