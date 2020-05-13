module Strategy.Clojure
  ( discover,
  )
where

import Control.Effect.Error
import qualified Data.EDN as EDN
import Data.EDN.Class.Parser (Parser)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Vector as V
import Discovery.Walk
import Effect.Exec
import Effect.Grapher
import Graphing (Graphing)
import Prologue
import Types

leinDepsCmd :: Command
leinDepsCmd =
  Command
    { cmdNames = ["lein"],
      cmdBaseArgs = ["deps", ":tree-data"],
      cmdAllowErr = Never
    }

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \dir _ files -> do
  case find (\f -> fileName f == "project.clj") files of
    Nothing -> pure WalkContinue
    Just file -> do
      runSimpleStrategy "clojure-lein" ClojureGroup $ mkProjectClosure dir <$> analyze file
      pure WalkSkipAll

mkProjectClosure :: Path Rel Dir -> Graphing Dependency -> ProjectClosureBody
mkProjectClosure dir deps =
  ProjectClosureBody
    { bodyModuleDir = dir,
      bodyDependencies =
        ProjectDependencies
          { dependenciesGraph = deps,
            dependenciesComplete = Complete,
            dependenciesOptimal = Optimal
          },
      bodyLicenses = []
    }

analyze :: (Has Exec sig m, Has (Error ExecErr) sig m) => Path Rel File -> m (Graphing Dependency)
analyze file = do
  stdoutBL <- execThrow (parent file) leinDepsCmd []
  let stdoutTL = decodeUtf8 stdoutBL
      stdout = TL.toStrict stdoutTL

  case EDN.decodeText "lein deps :tree-data" stdout of
    Left err -> throwError (CommandParseError "lein deps :tree-data" (T.pack err))
    Right deps -> pure (buildGraph deps)

data ClojureNode = ClojureNode
  { nodeName :: Text,
    nodeVersion :: Text
  }
  deriving (Eq, Ord, Show, Generic)

data ClojureLabel = ScopeLabel Text
  deriving (Eq, Ord, Show, Generic)

buildGraph :: Deps -> Graphing Dependency
buildGraph deps = run . withLabeling toDependency $ do
  traverse_ direct (topLevelNodes deps)
  buildEdges deps

-- extract the top-level clojure deps from a Deps as ClojureNodes
topLevelNodes :: Deps -> [ClojureNode]
topLevelNodes = map toClojureNode . M.keys . depsTree

buildEdges :: Has (LabeledGrapher ClojureNode ClojureLabel) sig m => Deps -> m ()
buildEdges deps = M.traverseWithKey single (depsTree deps) *> pure ()
  where
    single :: Has (LabeledGrapher ClojureNode ClojureLabel) sig m => ClojureDep -> Maybe Deps -> m ()
    single dep maybeDeeper = do
      let node = toClojureNode dep
      traverse_ (label node . ScopeLabel) (depScope dep)
      case maybeDeeper of
        Nothing -> pure ()
        Just deeper -> do
          traverse_ (edge node) (topLevelNodes deeper)
          buildEdges deeper

toClojureNode :: ClojureDep -> ClojureNode
toClojureNode dep = ClojureNode (depName dep) (depVersion dep)

toDependency :: ClojureNode -> Set ClojureLabel -> Dependency
toDependency node = foldr applyLabel start
  where
    start :: Dependency
    start =
      Dependency
        { dependencyType = MavenType,
          dependencyName = nodeName node,
          dependencyVersion = Just (CEq (nodeVersion node)),
          dependencyLocations = [],
          dependencyEnvironments = [],
          dependencyTags = M.empty
        }
    applyLabel (ScopeLabel "test") dep = insertEnvironment EnvTesting dep
    applyLabel (ScopeLabel other) dep = insertEnvironment (EnvOther other) dep

instance EDN.FromEDN Deps where
  parseEDNv = fmap Deps . EDN.parseEDNv

instance EDN.FromEDN ClojureDep where
  parseEDNv = EDN.withVec $ \vec -> do
    name <- EDN.vecGet 0 vec
    version <- EDN.vecGet 1 vec
    attributes <- ednVecToMap (V.drop 2 vec)

    scope <- optional (EDN.mapGetSymbol "scope" attributes)
    pure (ClojureDep name version scope)

-- in the input EDN, a dependency is a vector of values, like:
--
-- > [lein-koan "0.1.5" :scope "test"]
--
-- after the name/version are a bunch of key/value pairs, so we can turn them
-- into a Map to make them easier to work with. in the future, this can be
-- turned into an Map TaggedValue [TaggedValue] -- but for now, we only care
-- about scopes which can only appear once
ednVecToMap :: EDN.EDNVec -> Parser EDN.EDNMap
ednVecToMap = go M.empty
  where
    go :: EDN.EDNMap -> EDN.EDNVec -> Parser EDN.EDNMap
    go m vec
      | V.null vec = pure m
      | otherwise = do
        key <- EDN.vecGet 0 vec
        value <- EDN.vecGet 1 vec
        go (M.insert key value m) (V.drop 2 vec)

data Deps = Deps
  { depsTree :: Map ClojureDep (Maybe Deps)
  }
  deriving (Eq, Ord, Show, Generic)

data ClojureDep = ClojureDep
  { depName :: Text,
    depVersion :: Text,
    depScope :: Maybe Text
  }
  deriving (Eq, Ord, Show, Generic)
