{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Strategy.NuGet.Nuspec
  ( discover
  , discover'
  , buildGraph
  , analyze

  , Nuspec(..)
  , Group(..)
  , NuGetDependency(..)
  , NuspecLicense(..)

  , mkProjectClosure
  ) where

import Control.Applicative (optional)
import Control.Effect.Diagnostics
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (find)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing)
import qualified Graphing
import Parse.XML
import Path
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> L.isSuffixOf ".nuspec" (fileName f)) files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "nuget-nuspec" DotnetGroup $ analyze file

  pure WalkContinue

discover' :: MonadIO m => Path Abs Dir -> m [NewProject]
discover' dir = map mkProject <$> findProjects dir

findProjects :: MonadIO m => Path Abs Dir -> m [NuspecProject]
findProjects = walk' $ \_ _ files -> do
  case find (\f -> L.isSuffixOf ".nuspec" (fileName f)) files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([NuspecProject file], WalkContinue)

data NuspecProject = NuspecProject
  { nuspecFile :: Path Abs File
  }
  deriving (Eq, Ord, Show)

mkProject :: NuspecProject -> NewProject
mkProject project =
  NewProject
    { projectType = "nuspec",
      projectBuildTargets = mempty,
      projectDependencyGraph = const . runReadFSIO $ getDeps project,
      projectPath = parent $ nuspecFile project,
      projectLicenses = runReadFSIO $ analyzeLicenses (nuspecFile project)
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => NuspecProject -> m (Graphing Dependency)
getDeps = analyze' . nuspecFile

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze' file = buildGraph <$> readContentsXML @Nuspec file

analyzeLicenses :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m [LicenseResult]
analyzeLicenses file = do
  nuspec <- readContentsXML @Nuspec file
  pure [LicenseResult (toFilePath file) (nuspecLicenses nuspec)]

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m ProjectClosureBody
analyze file = mkProjectClosure file <$> readContentsXML @Nuspec file

mkProjectClosure :: Path Abs File -> Nuspec -> ProjectClosureBody
mkProjectClosure file nuspec = ProjectClosureBody
  { bodyModuleDir    = parent file
  , bodyDependencies = dependencies
  , bodyLicenses     = [LicenseResult (toFilePath file) (nuspecLicenses nuspec)]
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph nuspec
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

nuspecLicenses :: Nuspec -> [License]
nuspecLicenses nuspec = url ++ licenseField
          where
            url = case licenseUrl nuspec of
              Just location -> [License LicenseURL location]
              Nothing -> []
            licenseField = foldr (\a b -> b ++ [License (parseLicenseType $ nuspecLicenseType a) (nuspecLicenseValue a)]) [] (license nuspec)

parseLicenseType :: Text -> LicenseType
parseLicenseType rawType = case T.unpack rawType of 
                            "expression" -> LicenseSPDX
                            "file" -> LicenseFile
                            _ -> UnknownType

data Nuspec = Nuspec
  { groups        :: [Group]
  , license       :: [NuspecLicense]
  , licenseUrl    :: Maybe Text
  } deriving (Eq, Ord, Show)

data NuspecLicense = NuspecLicense
  { nuspecLicenseType  :: Text
  , nuspecLicenseValue :: Text
  } deriving (Eq, Ord, Show)

newtype Group = Group
  { dependencies  :: [NuGetDependency]
  } deriving (Eq, Ord, Show)

data NuGetDependency = NuGetDependency
  { depID      :: Text
  , depVersion :: Text
  } deriving (Eq, Ord, Show)

instance FromXML Nuspec where
  parseElement el = do
    metadata     <- child "metadata" el
    Nuspec <$> optional (child "dependencies" metadata >>= children "group") `defaultsTo` []
           <*> children "license" metadata
           <*> optional (child "licenseUrl" metadata)

instance FromXML NuspecLicense where
  parseElement el =
    NuspecLicense <$> optional (attr "type" el) `defaultsTo` ""
                  <*> content el

instance FromXML Group where
  parseElement el = Group <$> (children "dependency" el)

instance FromXML NuGetDependency where
  parseElement el =
    NuGetDependency <$> attr "id" el
                    <*> attr "version" el

buildGraph :: Nuspec -> Graphing Dependency
buildGraph project = Graphing.fromList (map toDependency direct)
    where
    direct = concatMap dependencies (groups project)
    toDependency NuGetDependency{..} =
      Dependency { dependencyType = NuGetType
                 , dependencyName = depID
                 , dependencyVersion = Just (CEq depVersion)
                 , dependencyLocations = []
                 , dependencyEnvironments = []
                 , dependencyTags = M.empty
                 }
