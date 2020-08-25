{-# language TemplateHaskell #-}

module Strategy.Composer
  ( discover
  , analyze
  , buildGraph

  , ComposerLock(..)
  , CompDep(..)
  , Source(..)
  )
  where

import Prologue

import Control.Effect.Diagnostics
import qualified Data.Map.Strict as M
import DepTypes
import Discovery.Walk
import Effect.Grapher
import Effect.ReadFS
import Graphing (Graphing)
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> fileName f == "composer.lock") files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "php-composerlock" PHPGroup $ analyze file

  pure WalkContinue

data ComposerLock = ComposerLock
  { packages    :: [CompDep]
  , packagesDev :: [CompDep]
  } deriving (Eq, Ord, Show, Generic)

data CompDep = CompDep
  { name         :: Text
  , version      :: Text
  , source       :: Source
  , require      :: Maybe (Map Text Text) -- ^ name to version spec
  , requireDev   :: Maybe (Map Text Text)
  } deriving (Eq, Ord, Show, Generic)

data Source = Source
  { sourceType  :: Text
  , url         :: Text
  , reference   :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON ComposerLock where
  parseJSON = withObject "ComposerLock" $ \obj ->
    ComposerLock <$> obj .: "packages"
                 <*> obj .: "packages-dev"

instance FromJSON CompDep where
  parseJSON = withObject "CompDep" $ \obj ->
    CompDep <$> obj .:  "name"
            <*> obj .:  "version"
            <*> obj .:  "source"
            <*> obj .:? "require"
            <*> obj .:? "require-dev"

instance FromJSON Source where
  parseJSON = withObject "Source" $ \obj ->
    Source <$> obj .: "type"
           <*> obj .: "url"
           <*> obj .: "reference"

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m ProjectClosureBody
analyze file = mkProjectClosure file <$> readContentsJson @ComposerLock file

mkProjectClosure :: Path Abs File -> ComposerLock -> ProjectClosureBody
mkProjectClosure file lock = ProjectClosureBody
  { bodyModuleDir    = parent file
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph lock
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = Complete
    }

newtype CompPkg = CompPkg { pkgName :: Text }
  deriving (Eq, Ord, Show, Generic)

type CompGrapher = LabeledGrapher CompPkg CompLabel

data CompLabel =
    CompVersion Text
    | CompEnv DepEnvironment
  deriving (Eq, Ord, Show, Generic)

buildGraph :: ComposerLock -> Graphing Dependency
buildGraph composerLock = run . withLabeling toDependency $ do
      traverse_ (addDeps EnvProduction) $ packages composerLock
      traverse_ (addDeps EnvDevelopment) $ packagesDev composerLock

  where
  addDeps :: Has CompGrapher sig m => DepEnvironment -> CompDep -> m ()
  addDeps env dep = do
        let pkg = CompPkg (name dep)
        _ <- M.traverseWithKey (addEdge pkg) (fromMaybe (M.fromList []) $ require dep) 
        label pkg (CompVersion $ version dep)
        label pkg (CompEnv env)
        direct pkg
  
  addEdge :: Has CompGrapher sig m => CompPkg -> Text -> Text -> m ()
  addEdge pkg name _ = do
        edge pkg (CompPkg name)

  toDependency :: CompPkg -> Set CompLabel -> Dependency
  toDependency pkg = foldr addLabel (start pkg)

  addLabel :: CompLabel -> Dependency -> Dependency
  addLabel (CompVersion ver) dep = dep { dependencyVersion = Just (CEq ver) }
  addLabel (CompEnv env) dep = dep { dependencyEnvironments = env : dependencyEnvironments dep }

  start :: CompPkg -> Dependency
  start CompPkg{..} = Dependency
    { dependencyType = ComposerType
    , dependencyName = pkgName
    , dependencyVersion = Nothing
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = M.empty
    }