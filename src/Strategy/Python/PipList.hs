module Strategy.Python.PipList
  ( discover
  , analyze

  , PipListDep(..)
  , buildGraph
  )
  where

import Prologue

import qualified Data.Map.Strict as M
import Polysemy
import Polysemy.Error
import Polysemy.Output

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

discover' :: Members '[Embed IO, Exec, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \dir _ files ->
  case find (\f -> fileName f `elem` ["setup.py", "requirements.txt"]) files of
    Nothing -> walkContinue
    Just _ -> do
      res <- runError @ExecErr (analyze dir)
      traverse_ (output . dummyConfigure "python-piplist" NotOptimal NotComplete dir) res
      walkContinue

pipListCmd :: Command
pipListCmd = Command
  { cmdNames = ["pip3", "pip"]
  , cmdBaseArgs = ["list", "--format=json"]
  , cmdAllowErr = Never
  }

analyze :: Members '[Exec, Error ExecErr] r => Path Rel Dir -> Sem r (Graphing Dependency)
analyze dir = do
  deps <- execJson @[PipListDep] dir pipListCmd []
  pure (buildGraph deps)

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
