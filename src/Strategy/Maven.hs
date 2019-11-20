module Strategy.Maven
  ( discover
  , strategy
  , analyze
  , buildGraph
  ) where

import Prologue

import qualified Data.Map.Strict as M
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Polysemy.Resource

import           Diagnostics
import           Discovery.Walk
import           Effect.Exec
import           Effect.GraphBuilder
import           Effect.ReadFS
import qualified Graph as G
import           Strategy.Maven.Plugin
import           Types

discover :: Discover
discover = Discover
  { discoverName = "maven"
  , discoverFunc = discover'
  }

mavenValidateCmd :: Command
mavenValidateCmd = Command
  { cmdNames = ["mvn"]
  , cmdBaseArgs = ["validate"]
  , cmdAllowErr = Never
  }

discover' :: forall r. Members '[Embed IO, Exec, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \dir subdirs files -> do
  let buildscripts = filter (\f -> fileName f == "pom.xml") files

  if null buildscripts
    then walkContinue
    else do
      res <- exec dir mavenValidateCmd []
      case res of
        Left _ -> walkContinue
        Right _ -> do
          output (configure dir)
          walkSkipAll subdirs

strategy :: Strategy BasicDirOpts
strategy = Strategy
  { strategyName = "maven-cli"
  , strategyAnalyze = analyze
  , strategyModule = targetDir
  , strategyOptimal = Optimal
  , strategyComplete = Complete
  }

analyze :: Members '[Embed IO, Resource, Exec, Error ExecErr, ReadFS, Error ReadFSErr] r => BasicDirOpts -> Sem r G.Graph
analyze BasicDirOpts{..} = withUnpackedPlugin $ \filepath -> do
  installPlugin targetDir filepath
  execPlugin targetDir
  pluginOutput <- parsePluginOutput targetDir
  pure (buildGraph pluginOutput)

buildGraph :: PluginOutput -> G.Graph
buildGraph PluginOutput{..} = run $ evalGraphBuilder G.empty $ do
  let byNumeric :: Map Int Artifact
      byNumeric = indexBy artifactNumericId outArtifacts

  (refsByNumeric :: Map Int G.DepRef) <- traverse (addNode . toDependency) byNumeric

  traverse_ (visitEdge refsByNumeric) outEdges

  where

  toDependency :: Artifact -> G.Dependency
  toDependency Artifact{..} = G.Dependency
    { dependencyType = G.MavenType
    , dependencyName = artifactGroupId <> ":" <> artifactArtifactId
    , dependencyVersion = Just (G.CEq artifactVersion)
    , dependencyLocations = []
    , dependencyTags = M.fromList $
      [("scopes", artifactScopes)] ++
      [("optional", ["true"]) | artifactOptional]
    }

  visitEdge refsByNumeric Edge{..} = do
    let refs = do
          parentRef <- M.lookup edgeFrom refsByNumeric
          childRef <- M.lookup edgeTo refsByNumeric
          Just (parentRef, childRef)

    traverse_ (uncurry addEdge) refs

  indexBy :: Ord k => (v -> k) -> [v] -> Map k v
  indexBy f = M.fromList . map (\v -> (f v, v))

configure :: Path Rel Dir -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicDirOpts
