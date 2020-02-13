module Strategy.NuGet.PackageReference
  ( discover
  , buildGraph
  , analyze

  , PackageReference(..)
  , ItemGroup(..)
  , Package(..)
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.List as L
import Polysemy
import Polysemy.Error
import Polysemy.Output

import Diagnostics
import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing, unfold)
import Parse.XML
import Types

discover :: Discover
discover = Discover
  { discoverName = "packagereference"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, ReadFS, Output ProjectClosure] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  case find isPackageRefFile files of
    Nothing -> pure ()
    Just file -> do
      res <- runError @ReadFSErr (analyze file)
      traverse_ output res

  walkContinue
 
  where 
      isPackageRefFile :: Path Rel File -> Bool
      isPackageRefFile file = any (\x -> L.isSuffixOf x (fileName file)) [".csproj", ".xproj", ".vbproj", ".dbproj", ".fsproj"]

analyze :: Members '[ReadFS, Error ReadFSErr] r => Path Rel File -> Sem r ProjectClosure
analyze file = mkProjectClosure file <$> readContentsXML @PackageReference file

mkProjectClosure :: Path Rel File -> PackageReference -> ProjectClosure
mkProjectClosure file package = ProjectClosure
  { closureStrategyGroup = DotnetGroup
  , closureStrategyName  = "nuget-packagereference"
  , closureModuleDir     = parent file
  , closureDependencies  = dependencies
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph package
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

newtype PackageReference = PackageReference
  { groups :: [ItemGroup]
  } deriving (Eq, Ord, Show, Generic)

newtype ItemGroup = ItemGroup
  { dependencies :: [Package]
  } deriving (Eq, Ord, Show, Generic)

data Package = Package
  { depID      :: Text
  , depVersion :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

instance FromXML PackageReference where
  parseElement el = PackageReference <$> children "ItemGroup" el

instance FromXML ItemGroup where
  parseElement el = ItemGroup <$> children "PackageReference" el

instance FromXML Package where
  parseElement el =
    Package <$> attr "Include" el
            <*> optional (child "Version" el)

buildGraph :: PackageReference -> Graphing Dependency
buildGraph project = unfold direct (const []) toDependency
    where
    direct = concatMap dependencies (groups project)
    toDependency Package{..} =
      Dependency { dependencyType = NuGetType
               , dependencyName = depID
               , dependencyVersion =  fmap CEq depVersion
               , dependencyLocations = []
               , dependencyTags = M.empty
               }
