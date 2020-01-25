module Strategy.Maven.Pom
  ( discover
  , strategy
  , analyze
  , buildGraph
  ) where

import Prologue

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import           Path.IO
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Polysemy.State

import           Diagnostics
import           Discovery.Walk
import           DepTypes
import           Effect.Error (try)
import           Effect.Exec
import           Effect.ReadFS
import           Graphing
import           Parse.XML
import           Types

discover :: Discover
discover = Discover
  { discoverName = "maven-pom"
  , discoverFunc = discover'
  }

discover' :: forall r. Members '[Embed IO, Exec, ReadFS, Error ReadFSErr, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' dir = do
  let doIt :: Members '[Embed IO, Output (Path Rel File)] r' => Sem r' ()
      doIt = flip walk dir $ \_ _ files -> do
        case find ((== "pom.xml") . fileName) files of
          Just file -> output file
          Nothing -> pure ()
        walkContinue

  (pomPaths, ()) <- outputToIOMonoid @(Path Rel File) Seq.singleton doIt
  -- TODO: exceptions
  absolutePomPaths <- traverse (embed @IO . makeAbsolute) pomPaths
  let resolvedPaths = map ResolvedModulePath (toList absolutePomPaths)

  hydratedGraph <- unfoldMavenGraph resolvedPaths

  undefined

dfsM :: (Ord a, Monad m) => (a -> m [a]) -> [a] -> m ()
dfsM f start = go S.empty start
  where
  go seen (x:xs)
    | S.member x seen = go seen xs
    | otherwise = do
        next <- f x
        go (S.insert x seen) (next ++ xs)
  go _    [] = pure ()

-- computation state: ( graph ResolvedModulePath * map resolvedmavenmodule (Either err pom) )
-- mavenmodule: ( path * (Either err pom) )
-- return: graph mavenmodule
unfoldMavenGraph :: Members '[ReadFS, Error ReadFSErr] r => [ResolvedModulePath] -> Sem r (AM.AdjacencyMap MavenModule)
unfoldMavenGraph modules = do
  (pomMap, (fileGraph, ())) <- runState @(Map ResolvedModulePath (Either ReadFSErr Pom)) M.empty $ runState @(AM.AdjacencyMap ResolvedModulePath) AM.empty $ dfsM go modules
  undefined

  where
  go :: Members '[ ReadFS
                 , Error ReadFSErr
                 , State (Map ResolvedModulePath (Either ReadFSErr Pom))
                 , State (AM.AdjacencyMap ResolvedModulePath)
                 ] r => ResolvedModulePath -> Sem r [ResolvedModulePath]
  go resolvedPath = do
    (maybePom :: Either ReadFSErr Pom) <- try (readContentsXML (resolvedModulePath resolvedPath))
    modify (M.insert resolvedPath maybePom)
    case maybePom of
      Left _ -> pure []
      Right pom -> do
        -- TODO: resolve parent + submodules
        for_ (pomParent pom) $ \mvnParent -> do
          undefined

        for_ (pomModules pom) $ \submodules -> do
          undefined

        -- TODO: add edges between resolved paths
        -- TODO: pass out resolved paths
        undefined

newtype ResolvedModulePath = ResolvedModulePath { resolvedModulePath ::  Path Abs File }
  deriving (Eq, Ord, Show, Generic)

-- TODO: have Ord/Eq key on just the file?
data MavenModule = MavenModule (Path Abs File) (Maybe Pom)
  deriving (Eq, Ord, Show, Generic)

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "maven-pom"
  , strategyAnalyze = analyze
  , strategyModule = parent . targetFile
  , strategyOptimal = Optimal
  , strategyComplete = Complete
  }

analyze :: Members '[ReadFS, Error ReadFSErr] r => BasicFileOpts -> Sem r (Graphing Dependency)
analyze BasicFileOpts{..} = undefined

data Pom = Pom
  { pomParent               :: Maybe Parent
  , pomModules              :: Maybe [Module]
  , pomDependencyManagement :: Maybe [MvnDependency]
  , pomDependencies         :: Maybe [MvnDependency]
  } deriving (Eq, Ord, Show, Generic)

data Parent = Parent
  { parentGroup        :: Maybe Text
  , parentArtifact     :: Text
  , parentVersion      :: Maybe Text
  , parentRelativePath :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

data MvnDependency = MvnDependency
  { depGroup      :: Text
  , depArtifact   :: Text
  , depVersion    :: Maybe Text
  , depClassifier :: Maybe Text
  , depScope      :: Maybe Text
  , depOptional   :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

newtype Module = Module { modulePath :: Text }
  deriving (Eq, Ord, Show, Generic)


instance FromXML Pom where
  parseElement el = do
    Pom <$> optional (child "parent" el)
        <*> optional (child "modules" el >>= children "module")
        <*> optional (child "dependencyManagement" el >>= children "dependency")
        <*> optional (child "dependencies" el >>= children "dependency")

instance FromXML Parent where
  -- "although, groupId and version need not be explicitly defined if they are inherited from a parent"
  -- https://maven.apache.org/pom.html#Maven_Coordinates
  -- "Notice the relativePath element. It is not required, but may be used as a signifier to Maven to first
  -- search the path given for this project's parent, before searching the local and then remote repositories."
  -- https://maven.apache.org/pom.html#Inheritance
  parseElement el =
    Parent <$> optional (child "groupId" el)
           <*> child "artifactId" el
           <*> optional (child "version" el)
           <*> optional (child "relativePath" el)

instance FromXML Module where
  parseElement = fmap Module . parseElement

instance FromXML MvnDependency where
  parseElement el =
    MvnDependency <$> child "groupId" el
                  <*> child "artifactId" el
                  <*> optional (child "version" el)
                  <*> optional (child "classifier" el)
                  <*> optional (child "scope" el)
                  <*> optional (child "optional" el)

buildGraph = undefined

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts
