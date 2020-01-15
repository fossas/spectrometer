module Strategy.NuGet.Nuspec
  ( discover
  , strategy
  , buildGraph
  , analyze
  , parseNuspec

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
  { discoverName = "nuspec"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  case find (\f -> L.isSuffixOf ".nuspec" (fileName f)) files of
    Just file -> output (configure file)
    Nothing -> pure ()

  walkContinue

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "nuget-nuspec"
  , strategyAnalyze = analyze
  , strategyModule = parent . targetFile
  , strategyOptimal = NotOptimal
  , strategyComplete = NotComplete
  }

analyze :: Members '[ReadFS, Error ReadFSErr] r => BasicFileOpts -> Sem r (Graphing Dependency)
analyze BasicFileOpts{..} = do
      contents <- readContentsBS targetFile
      let nuspecGroups = parseNuspec =<< XML.parseXMLDoc contents
      
      case nuspecGroups of
        Just gs -> pure $ buildGraph gs
        Nothing -> undefined

data Group = Group
  { dependencies  :: [NuGetDependency]
  } deriving (Eq, Ord, Show, Generic)

data NuGetDependency = NuGetDependency
  { depID        :: String
  , depVersion   :: String
  } deriving (Eq, Ord, Show, Generic)

parseNuspec :: XML.Element -> Maybe [Group]
parseNuspec nuspec = do
  guard (XML.qName (XML.elName nuspec) == "package")
  parseGroups =<< childByName "metadata" nuspec
  where

  parseGroups :: XML.Element -> Maybe [Group]
  parseGroups el = traverse parseGroup . childrenByName "group" =<< childByName "dependencies" el

  parseGroup :: XML.Element -> Maybe Group
  parseGroup el = fmap Group $ traverse parseDependency $ childrenByName "dependency" el

  parseDependency :: XML.Element -> Maybe NuGetDependency
  parseDependency el = do
    depIds  <- attrByName "id" el
    version <- attrByName "version" el

    pure (NuGetDependency depIds version)

  attrByName :: String -> XML.Element -> Maybe String
  attrByName name element = XML.findAttrBy (\elName -> XML.qName elName == name) element 

  childByName :: String -> XML.Element -> Maybe XML.Element
  childByName name = XML.filterChildName (\elName -> XML.qName elName == name)

  childrenByName :: String -> XML.Element -> [XML.Element]
  childrenByName name = XML.filterChildrenName (\elName -> XML.qName elName == name)

buildGraph :: [Group] -> Graphing Dependency
buildGraph project = unfold direct (const []) toDependency
    where
    direct = foldr (\x y -> y ++ dependencies x) [] project
    toDependency NuGetDependency{..} =
      Dependency { dependencyType = NuGetType
               , dependencyName = T.pack depID
               , dependencyVersion = Just (CEq $ T.pack depVersion)
               , dependencyLocations = []
               , dependencyTags = M.empty
               }

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts