module Strategy.Maven.PluginStrategy
  ( discover
  , analyze
  , buildGraph
  ) where

import Prologue

import qualified Data.Map.Strict as M
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Polysemy.Resource

import DepTypes
import Diagnostics
import Discovery.Walk
import Effect.Exec
import Effect.Grapher hiding (Edge)
import Effect.ReadFS
import Graphing (Graphing)
import Strategy.Maven.Plugin
import Types

discover :: Discover
discover = Discover
  { discoverName = "maven"
  , discoverFunc = discover'
  }

discover' :: forall r. Members '[Embed IO, Resource, Exec, ReadFS, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ subdirs files -> do
  case find (\f -> fileName f == "pom.xml") files of
    Nothing -> walkContinue
    Just file -> do
      maybeRes <- runError @ReadFSErr $ runError @ExecErr (analyze (parent file))
      case maybeRes of
        Right (Right res) -> output (dummyConfigure "maven-cli" Optimal Complete (parent file) res)
        _ -> pure ()
      walkSkipAll subdirs

analyze :: Members '[Embed IO, Resource, Exec, Error ExecErr, ReadFS, Error ReadFSErr] r => Path Rel Dir -> Sem r (Graphing Dependency)
analyze dir = withUnpackedPlugin $ \filepath -> do
  installPlugin dir filepath
  execPlugin dir
  pluginOutput <- parsePluginOutput dir
  pure (buildGraph pluginOutput)

buildGraph :: PluginOutput -> Graphing Dependency
buildGraph PluginOutput{..} = run $ evalGrapher $ do
  let byNumeric :: Map Int Artifact
      byNumeric = indexBy artifactNumericId outArtifacts

  let depsByNumeric :: Map Int Dependency
      depsByNumeric = M.map toDependency byNumeric

  traverse_ (visitEdge depsByNumeric) outEdges

  where

  toDependency :: Artifact -> Dependency
  toDependency Artifact{..} = Dependency
    { dependencyType = MavenType
    , dependencyName = artifactGroupId <> ":" <> artifactArtifactId
    , dependencyVersion = Just (CEq artifactVersion)
    , dependencyLocations = []
    , dependencyTags = M.fromList $
      ("scopes", artifactScopes) :
      [("optional", ["true"]) | artifactOptional]
    }

  visitEdge :: Member (Grapher Dependency) r => Map Int Dependency -> Edge -> Sem r ()
  visitEdge refsByNumeric Edge{..} = do
    let refs = do
          parentRef <- M.lookup edgeFrom refsByNumeric
          childRef <- M.lookup edgeTo refsByNumeric
          Just (parentRef, childRef)

    traverse_ (uncurry edge) refs

  indexBy :: Ord k => (v -> k) -> [v] -> Map k v
  indexBy f = M.fromList . map (\v -> (f v, v))
