module Strategy.NpmList
  ( discover
  , analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import Polysemy
import Polysemy.Error
import Polysemy.Output

import Diagnostics
import DepTypes
import Discovery.Walk
import Effect.Exec
import Graphing (Graphing, unfold)
import Types

discover :: Discover
discover = Discover
  { discoverName = "npm-list"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Exec, Output ProjectClosure] r => Path Abs Dir -> Sem r ()
discover' = walk $ \dir subdirs files -> do
  case find (\f -> fileName f == "package.json") files of
    Nothing -> pure ()
    Just _ -> do
      res <- runError @ExecErr (analyze dir)
      traverse_ output res

  walkSkipNamed ["node_modules/"] subdirs

npmListCmd :: Command
npmListCmd = Command
  { cmdNames = ["npm"]
  , cmdBaseArgs = ["ls", "--json", "--production"]
  , cmdAllowErr = NonEmptyStdout
  }

analyze :: Members '[Exec, Error ExecErr] r => Path Rel Dir -> Sem r ProjectClosure
analyze dir = do
  npmOutput <- execJson @NpmOutput dir npmListCmd []
  pure (mkProjectClosure dir npmOutput)

mkProjectClosure :: Path Rel Dir -> NpmOutput -> ProjectClosure
mkProjectClosure dir npmOutput = ProjectClosure
  { closureStrategyGroup = NodejsGroup
  , closureStrategyName  = "nodejs-npmlist"
  , closureModuleDir     = dir
  , closureDependencies  = dependencies
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph npmOutput
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = Complete
    }

buildGraph :: NpmOutput -> Graphing Dependency
buildGraph top = unfold direct getDeps toDependency
  where
  direct = M.toList $ outputDependencies top
  getDeps (_,nodeOutput) = M.toList $ outputDependencies nodeOutput
  toDependency (nodeName, nodeOutput) =
    Dependency { dependencyType = NodeJSType
               , dependencyName = nodeName
               , dependencyVersion = CEq <$> outputVersion nodeOutput
               , dependencyLocations = []
               , dependencyTags = M.empty
               }

data NpmOutput = NpmOutput
  { outputInvalid      :: Maybe Bool
  , outputVersion      :: Maybe Text
  , outputFrom         :: Maybe Text
  , outputResolved     :: Maybe Text
  , outputDependencies :: Map Text NpmOutput
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON NpmOutput where
  parseJSON = withObject "NpmOutput" $ \obj ->
    NpmOutput <$> obj .:? "invalid"
              <*> obj .:? "version"
              <*> obj .:? "from"
              <*> obj .:? "resolved"
              <*> obj .:? "dependencies" .!= M.empty
