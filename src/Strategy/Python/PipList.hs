module Strategy.Python.PipList
  ( discover
  , analyze

  , PipListDep(..)
  , buildGraph
  )
  where

import Prologue

import Control.Carrier.Lift
import Control.Carrier.Error.Either
import Control.Carrier.Writer.Strict
import qualified Data.Map.Strict as M

import Diagnostics
import DepTypes
import Discovery.Walk
import Effect.Exec
import Graphing
import Types

discover :: Discover
discover = Discover
  { discoverName = "piplist"
  , discoverFunc = discover'
  }

discover' ::
  ( Has Exec sig m
  , Has (Writer [ProjectClosure]) sig m
  , MonadIO m
  , Effect sig
  )
  => Path Abs Dir -> m ()
discover' = walk $ \dir _ files ->
  case find (\f -> fileName f `elem` ["setup.py", "requirements.txt"]) files of
    Nothing -> walkContinue
    Just _ -> do
      res <- runError @ExecErr (analyze dir)
      traverse_ (tell @[ProjectClosure] . pure) res
      walkContinue

pipListCmd :: Command
pipListCmd = Command
  { cmdNames = ["pip3", "pip"]
  , cmdBaseArgs = ["list", "--format=json"]
  , cmdAllowErr = Never
  }

analyze :: (Has Exec sig m, Has (Error ExecErr) sig m) => Path Rel Dir -> m ProjectClosure
analyze dir = mkProjectClosure dir <$> execJson @[PipListDep] dir pipListCmd []

mkProjectClosure :: Path Rel Dir -> [PipListDep] -> ProjectClosure
mkProjectClosure dir deps = ProjectClosure
  { closureStrategyGroup = PythonGroup
  , closureStrategyName  = "python-piplist"
  , closureModuleDir     = dir
  , closureDependencies  = dependencies
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph deps
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

buildGraph :: [PipListDep] -> Graphing Dependency
buildGraph xs = unfold xs (const []) toDependency
  where
  toDependency PipListDep{..} =
    Dependency { dependencyType = PipType
               , dependencyName = depName
               , dependencyVersion = Just (CEq depVersion)
               , dependencyLocations = []
               , dependencyTags = M.empty
               }

data PipListDep = PipListDep
  { depName    :: Text
  , depVersion :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PipListDep where
  parseJSON = withObject "PipListDep" $ \obj ->
    PipListDep <$> obj .: "name"
               <*> obj .: "version"
