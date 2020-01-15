module Strategy.NuGet.PackageReference
  ( discover
  , strategy
  , buildGraph
  , analyze
  , parsePackageReference

  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.List as L
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import qualified Text.XML.Light as XML

import           Diagnostics
import           DepTypes
import           Discovery.Walk
import           Effect.ReadFS
import           Graphing (Graphing, unfold)
import           Types

discover :: Discover
discover = Discover
  { discoverName = "packagereference"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  case find isPackageRefFile files of
    Just file -> output (configure file)
    Nothing -> pure ()

  walkContinue
 
  where 
      isPackageRefFile :: Path Rel File -> Bool
      isPackageRefFile file = any (\x -> L.isSuffixOf x (fileName file)) [".csproj", ".xproj", ".vbproj", ".dbproj", ".fsproj"]

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "nuget-packagereference"
  , strategyAnalyze = analyze
  , strategyModule = parent . targetFile
  , strategyOptimal = NotOptimal
  , strategyComplete = NotComplete
  }

analyze :: Members '[ReadFS, Error ReadFSErr] r => BasicFileOpts -> Sem r (Graphing Dependency)
analyze BasicFileOpts{..} = do
      contents <- readContentsBS targetFile
      let packageReference = parsePackageReference =<< XML.parseXMLDoc contents
      
      case packageReference of
        Just gs -> pure $ buildGraph gs
        Nothing -> undefined

data ItemGroup = ItemGroup
  { dependencies        :: [PackageReference]
  } deriving (Eq, Ord, Show, Generic)

data PackageReference = PackageReference
  { depID        :: String
  , depVersion   :: Maybe String
  } deriving (Eq, Ord, Show, Generic)

parsePackageReference :: XML.Element -> Maybe [ItemGroup]
parsePackageReference project = do
  guard (XML.qName (XML.elName project) == "Project")
  traverse parseItemGroup $ childrenByName "ItemGroup" project
  where

  parseItemGroup :: XML.Element -> Maybe ItemGroup
  parseItemGroup el = fmap ItemGroup $ traverse parsePackage $ childrenByName "PackageReference" el 

  parsePackage :: XML.Element -> Maybe PackageReference
  parsePackage el = do
    include <- attrByName "Include" el
    let version = stringByName "Version" el

    pure (PackageReference include version)

  stringByName :: String -> XML.Element -> Maybe String
  stringByName name = fmap XML.strContent . childByName name

  attrByName :: String -> XML.Element -> Maybe String
  attrByName name element = XML.findAttrBy (\elName -> XML.qName elName == name) element 

  childByName :: String -> XML.Element -> Maybe XML.Element
  childByName name = XML.filterChildName (\elName -> XML.qName elName == name)

  childrenByName :: String -> XML.Element -> [XML.Element]
  childrenByName name = XML.filterChildrenName (\elName -> XML.qName elName == name)

buildGraph :: [ItemGroup] -> Graphing Dependency
buildGraph project = unfold direct (const []) toDependency
    where
    direct = foldr (\x y -> y ++ dependencies x) [] project
    toDependency PackageReference{..} =
      Dependency { dependencyType = NuGetType
               , dependencyName = T.pack depID
               , dependencyVersion = case depVersion of
                                       Nothing -> Nothing
                                       Just ver -> Just (CEq $ T.pack ver)
               , dependencyLocations = []
               , dependencyTags = M.empty
               }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts