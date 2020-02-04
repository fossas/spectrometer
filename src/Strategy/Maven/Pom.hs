-- FIXME: remove?
{-# language TemplateHaskell #-}

module Strategy.Maven.Pom
  ( discover
  , strategy
  ) where

import Prologue

import qualified Algebra.Graph.AdjacencyMap as AM
import           Data.List ((\\))
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Path.IO as PIO
import           Polysemy
import           Polysemy.Error
import           Polysemy.NonDet
import           Polysemy.Output
import           Polysemy.State

import           Diagnostics
import           Discovery.Walk
import           DepTypes
import           Effect.Error (try)
import           Effect.Exec
import           Effect.LabeledGrapher
import           Effect.ReadFS
import           Graphing
import           Parse.XML
import           Types

-- > although, groupId and version need not be explicitly defined if they are inherited from a parent
-- > The relative path of the parent <code>pom.xml</code> file within the check out. If not specified, it defaults to <code>../pom.xml</code>
-- > However, the group ID, artifact ID and version are still required, and must match the file in the location given or it will revert to the repository for the POM.

-- resolve a Filepath (in Text) that may either point to a directory or an exact
-- pom file. when it's a directory, we default to pointing at the "pom.xml" in
-- that directory.
resolvePath :: Members '[ReadFS, Error ReadFSErr] r => Path Abs Dir -> Text -> Sem r (Path Abs File)
resolvePath cur txt = nonDetToError (ResolveError "Resolved file doesn't exist") $ do
  let resolveToFile :: Members '[NonDet, ReadFS, Error ReadFSErr] r => Sem r (Path Abs File)
      resolveToFile = do
        file <- resolveFile cur txt
        exists <- doesFileExist file
        guard exists
        pure file

      resolveToDir :: Members '[NonDet, ReadFS, Error ReadFSErr] r => Sem r (Path Abs File)
      resolveToDir = do
        dir <- resolveDir cur txt
        let file = dir </> $(mkRelFile "pom.xml")
        exists <- doesFileExist file
        guard exists
        pure file

  resolveToFile <|> resolveToDir

data MavenResolveReq = MavenResolveReq (Path Abs Dir) Text
  deriving (Eq, Ord, Show, Generic)

newtype ResolvedModulePath = ResolvedModulePath { resolvedModulePath ::  Path Abs File }
  deriving (Eq, Ord, Show, Generic)

data MavenCoordinate = MavenCoordinate
  { coordGroup    :: Text
  , coordArtifact :: Text
  , coordVersion  :: Text
  } deriving (Eq, Ord, Show, Generic)

-- TODO: have Ord/Eq key on just the file?
data MavenModule = MavenModule (Path Abs File) (Maybe Pom)
  deriving (Eq, Ord, Show, Generic)

data MavenStrategyOpts = MavenStrategyOpts
  { strategyPath  :: Path Rel File
  , strategyGraph :: Graphing Dependency
  } deriving (Eq, Ord, Show, Generic)

strategy :: Strategy MavenStrategyOpts
strategy = Strategy
  { strategyName = "maven-pom"
  , strategyAnalyze = pure . strategyGraph
  , strategyModule = parent . strategyPath
  , strategyOptimal = NotOptimal
  , strategyComplete = NotComplete
  }

-- TODO: newtypes around Text for better type safety? in particular: property interpolation
data Pom = Pom
  { pomParent               :: Maybe Parent
  , pomGroup                :: Maybe Text
  , pomArtifact             :: Text
  , pomVersion              :: Maybe Text
  , pomProperties           :: Map Text Text
  , pomModules              :: [Text]
  , pomDependencyManagement :: [MvnDependency]
  , pomDependencies         :: [MvnDependency]
  } deriving (Eq, Ord, Show, Generic)

data Parent = Parent
  { parentGroup        :: Text
  , parentArtifact     :: Text
  , parentVersion      :: Text
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

instance FromXML Pom where
  parseElement el = do
    Pom <$> optional (child "parent" el)
        <*> optional (child "groupId" el)
        <*> child "artifactId" el
        <*> optional (child "version" el)
        <*> optional (child "properties" el) `defaultsTo` M.empty
        <*> optional (child "modules" el >>= children "module") `defaultsTo` []
        <*> optional (child "dependencyManagement" el >>= children "dependency") `defaultsTo` []
        <*> optional (child "dependencies" el >>= children "dependency") `defaultsTo` []

instance FromXML Parent where
  -- "although, groupId and version need not be explicitly defined if they are inherited from a parent"
  -- https://maven.apache.org/pom.html#Maven_Coordinates
  -- "Notice the relativePath element. It is not required, but may be used as a signifier to Maven to first
  -- search the path given for this project's parent, before searching the local and then remote repositories."
  -- https://maven.apache.org/pom.html#Inheritance
  parseElement el =
    Parent <$> child "groupId" el
           <*> child "artifactId" el
           <*> child "version" el
           <*> optional (child "relativePath" el)

instance FromXML MvnDependency where
  parseElement el =
    MvnDependency <$> child "groupId" el
                  <*> child "artifactId" el
                  <*> optional (child "version" el)
                  <*> optional (child "classifier" el)
                  <*> optional (child "scope" el)
                  <*> optional (child "optional" el)

-- FIXME: gross
data PomCache m a where
  LoadPom :: Path Abs File -> PomCache m (Either ReadFSErr Pom)

makeSem ''PomCache

runPomCache :: Members '[ReadFS, Error ReadFSErr] r => Sem (PomCache ': r) a -> Sem r (Map (Path Abs File) (Either ReadFSErr Pom), a)
runPomCache = runState @(Map (Path Abs File) (Either ReadFSErr Pom)) M.empty . reinterpret
  (\case
      LoadPom path -> do
        (cached :: Maybe (Either ReadFSErr Pom)) <- gets (M.lookup path)
        case cached of
          -- FIXME: gross
          Just _ -> pure (Left (ResolveError "FIXME"))
          Nothing -> do
            (res :: Either ReadFSErr Pom) <- try (readContentsXML path)
            modify (M.insert path res)
            pure res
  )

recursiveLoadPom :: forall r. Members '[ReadFS, Error ReadFSErr, PomCache] r => Path Abs File -> Sem r ()
recursiveLoadPom path = do
  maybePom <- loadPom path
  case maybePom of
    Left _ -> pure () -- TODO: diagnostics/warnings?
    Right pom -> loadParent pom *> loadSubmodules pom

  where

  loadParent pom = for_ (pomParent pom) $ \mvnParent ->
    -- the default relative path is "../pom.xml"
    loop (fromMaybe "../pom.xml" (parentRelativePath mvnParent))

  loadSubmodules pom = traverse_ loop (pomModules pom)

  loop :: Text {- relative filepath -} -> Sem r ()
  loop rel = do
    (resolvedPath :: Either ReadFSErr (Path Abs File)) <- try (resolvePath (parent path) rel)
    -- TODO: diagnostics/warnings?
    traverse_ recursiveLoadPom resolvedPath

-- TODO: newtypes?
type Group = Text
type Artifact = Text
data ValidPom = ValidPom
  { reifiedCoord                :: MavenCoordinate
  , reifiedParent               :: Maybe MavenCoordinate
  , reifiedProperties           :: Map Text Text
  , reifiedDependencyManagement :: Map (Group, Artifact) MvnDependency
  , reifiedDependencies         :: Map (Group, Artifact) MvnDependency
  } deriving (Eq, Ord, Show, Generic)

instance Semigroup ValidPom where
  -- TODO: overlay properties and dependencyManagement
  (<>) = overlayPoms

-- overlaying poms:
-- - https://maven.apache.org/pom.html#Inheritance
-- TODO (incomplete list):
-- - dependencyManagement
-- - properties
-- - dependencies
overlayPoms :: ValidPom -> ValidPom -> ValidPom
overlayPoms parentPom childPom = undefined

-- TODO: turn `Map (Path Abs File) (Either ReadFSErr Pom)` into:
-- - Map MavenCoordinate (Path Abs File)
-- - AdjacencyMap MavenCoordinate
data GlobalClosure = GlobalClosure
  { globalGraph :: AM.AdjacencyMap MavenCoordinate
  , globalPoms  :: Map MavenCoordinate (Path Abs File, ValidPom)
  } deriving (Eq, Ord, Show, Generic)

buildClosure :: Map (Path Abs File) ValidPom -> GlobalClosure
buildClosure cache = GlobalClosure
  { globalGraph = AM.overlays
      [AM.edge parentCoord (reifiedCoord pom)
        | pom <- M.elems cache
        , Just parentCoord <- [reifiedParent pom]]
  , globalPoms = indexBy (reifiedCoord . snd) (M.toList cache)
  }

indexBy :: Ord k => (v -> k) -> [v] -> Map k v
indexBy f = M.fromList . map (\v -> (f v, v))

sourceVertices :: Ord a => AM.AdjacencyMap a -> [a]
sourceVertices graph = [v | v <- AM.vertexList graph, S.null (AM.preSet v graph)]

determineProjectRoots :: Path Abs Dir -> GlobalClosure -> [MavenCoordinate] -> Map (Path Rel File) (MavenCoordinate, ValidPom)
determineProjectRoots rootDir closure = go
  where
  go [] = M.empty
  go coordRoots = M.union projects (go frontier)
    where
    inRoot :: [(MavenCoordinate, Path Rel File, ValidPom)]
    inRoot = mapMaybe (\coord -> do
                          (abspath, pom) <- M.lookup coord (globalPoms closure)
                          relpath <- PIO.makeRelative rootDir abspath
                          pure (coord, relpath, pom)) coordRoots

    inRootCoords :: [MavenCoordinate]
    inRootCoords = map (\(c,_,_) -> c) inRoot

    remainingCoords :: [MavenCoordinate]
    remainingCoords = coordRoots \\ inRootCoords

    projects :: Map (Path Rel File) (MavenCoordinate, ValidPom)
    projects = M.fromList (map (\(coord,path,pom) -> (path, (coord, pom))) inRoot)

    frontier :: [MavenCoordinate]
    frontier = concatMap (\coord -> S.toList $ AM.postSet coord (globalGraph closure)) remainingCoords

-- TODO: use MavenCoordinate?
data MavenPackage = MavenPackage
  deriving (Eq, Ord, Show, Generic)

type instance PkgLabel MavenPackage = MavenLabel

data MavenLabel = MavenLabel
  deriving (Eq, Ord, Show, Generic)

-- TODO: overlay pom data
-- TODO: property interpolation
-- TODO: make topcoord direct
buildProjectGraph :: GlobalClosure -> MavenCoordinate -> ValidPom -> Graphing Dependency
buildProjectGraph closure topcoord toppom = run . withLabeling toDependency $ go topcoord toppom
  where
  go coord pom = undefined

  toDependency :: MavenPackage -> Set MavenLabel -> Dependency
  toDependency = undefined

reifyDeps :: ValidPom -> [MvnDependency]
reifyDeps = undefined

discover :: Discover
discover = Discover
  { discoverName = "maven-pom"
  , discoverFunc = discover'
  }

discover' :: forall r. Members '[Embed IO, Exec, ReadFS, Error ReadFSErr, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' dir = do
  let findPoms :: Members '[Embed IO, Output (Path Rel File)] r' => Sem r' ()
      findPoms = flip walk dir $ \_ _ files -> do
        case find ((== "pom.xml") . fileName) files of
          Just file -> output file
          Nothing -> pure ()
        walkContinue

  (pomPaths, ()) <- outputToIOMonoid @(Path Rel File) Seq.singleton findPoms
  -- FIXME: exceptions, Embed IO
  absolutePomPaths <- traverse (embed @IO . PIO.makeAbsolute) pomPaths

  (cache, ()) <- runPomCache $ traverse_ recursiveLoadPom absolutePomPaths

  let validated :: Map (Path Abs File) ValidPom
      validated = M.mapMaybe (validate <=< eitherToMaybe) cache

      eitherToMaybe :: Either a b -> Maybe b
      eitherToMaybe (Left _) = Nothing
      eitherToMaybe (Right b) = Just b

      validate :: Pom -> Maybe ValidPom
      validate pom = do
        coord <- validateCoordinate pom
        let parentCoord = parentToCoordinate <$> pomParent pom
            properties = pomProperties pom
            depManagement = indexBy (\dep -> (depGroup dep, depArtifact dep)) (pomDependencyManagement pom)
            deps = indexBy (\dep -> (depGroup dep, depArtifact dep)) (pomDependencies pom)
        pure (ValidPom coord parentCoord properties depManagement deps)
--
      validateCoordinate :: Pom -> Maybe MavenCoordinate
      validateCoordinate pom = MavenCoordinate
        <$> (pomGroup pom <|> (parentGroup <$> pomParent pom))
        <*> pure (pomArtifact pom)
        <*> (pomVersion pom <|> (parentVersion <$> pomParent pom))

      parentToCoordinate :: Parent -> MavenCoordinate
      parentToCoordinate Parent{..} = MavenCoordinate
        { coordGroup    = parentGroup
        , coordArtifact = parentArtifact
        , coordVersion  = parentVersion
        }

  let closure = buildClosure validated
      roots = sourceVertices (globalGraph closure)

      projectRoots :: Map (Path Rel File) (MavenCoordinate, ValidPom)
      projectRoots = determineProjectRoots dir closure roots

      projects :: Map (Path Rel File) (Graphing Dependency)
      projects = M.map (\(coord,pom) -> buildProjectGraph closure coord pom) projectRoots

  traverse_ (output . ConfiguredStrategy strategy . uncurry MavenStrategyOpts) (M.toList projects)
  pure ()
