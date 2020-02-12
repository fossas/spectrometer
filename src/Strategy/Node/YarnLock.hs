module Strategy.Node.YarnLock
  ( discover
  , strategy
  , analyze
  ) where

import Prologue

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.MultiKeyedMap as MKM
import Polysemy
import Polysemy.Error
import Polysemy.Output
import qualified Yarn.Lock as YL
import qualified Yarn.Lock.Types as YL

import DepTypes
import Diagnostics
import Discovery.Walk
import Effect.Grapher
import Effect.ReadFS
import Graphing (Graphing)
import Types

discover :: Discover
discover = Discover
  { discoverName = "yarn-lock"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, ReadFS, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ subdirs files -> do
  case find (\f -> fileName f == "yarn.lock") files of
    Nothing -> pure ()
    Just file -> do
      res <- runError @ReadFSErr (analyze file)
      traverse_ (output . dummyConfigure "nodejs-yarnlock" Optimal Complete (parent file)) res

  walkSkipNamed ["node_modules/"] subdirs

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "nodejs-yarnlock"
  , strategyAnalyze = analyze . targetFile
  , strategyModule = parent . targetFile
  , strategyOptimal = Optimal
  , strategyComplete = Complete
  }

analyze :: Members '[ReadFS, Error ReadFSErr] r => Path Rel File -> Sem r (Graphing Dependency)
analyze lockfile = do
  let path = fromRelFile lockfile

  contents <- readContentsText lockfile
  case YL.parse path contents of
    Left err -> throw (FileParseError path (YL.prettyLockfileError err))
    Right a -> pure (buildGraph a)

buildGraph :: YL.Lockfile -> Graphing Dependency
buildGraph lockfile = run . evalGrapher $
  traverse (add . first NE.head) (MKM.toList lockfile)
  where
  add :: Member (Grapher Dependency) r => (YL.PackageKey, YL.Package) -> Sem r ()
  add parentPkg@(_, package) =
    for_ (YL.dependencies package) $ \childKey -> do
      let childPkg = (childKey, MKM.at lockfile childKey)
      edge (toDependency parentPkg) (toDependency childPkg)

  toDependency (key,package) =
    Dependency { dependencyType = NodeJSType
               , dependencyName =
                   case YL.name key of
                     YL.SimplePackageKey name -> name
                     YL.ScopedPackageKey scope name -> scope <> "/" <> name
               , dependencyVersion = Just (CEq (YL.version package))
               , dependencyLocations =
                   case YL.remote package of
                     YL.FileLocal _ _ -> [] -- FUTURE: upload this for analysis?
                     YL.FileLocalNoIntegrity _ -> [] -- FUTURE: upload this for analysis?
                     YL.FileRemote url _ -> [url]
                     YL.FileRemoteNoIntegrity url -> [url]
                     YL.GitRemote url rev -> [url <> "@" <> rev]
               , dependencyTags = M.empty
               }
