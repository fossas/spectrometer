module Strategy.Swift.Xcode.Pbxproj (
  analyzeXcodeProjForSwiftPkg,
  hasSomeSwiftDeps,

  -- * for testing
  buildGraph,
  XCRemoteSwiftPackageReference (..),
  swiftPackageReferencesOf,
) where

import Control.Effect.Diagnostics (Diagnostics, context)
import Data.Foldable (asum)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (fromList, member)
import Data.Text (Text)
import DepTypes (DepType (GitType, SwiftType), Dependency (..))
import Effect.ReadFS (Has, ReadFS, readContentsJson, readContentsParser)
import Graphing (Graphing, deeps, directs, promoteToDirect)
import Path
import Strategy.Swift.PackageResolved (SwiftPackageResolvedFile, resolvedDependenciesOf)
import Strategy.Swift.PackageSwift (
  SwiftPackageGitDepRequirement (..),
  isGitRefConstraint,
  toConstraint,
 )
import Strategy.Swift.Xcode.PbxprojParser (AsciiValue (..), PbxProj (..), lookupText, objectsFromIsa, parsePbxProj, textOf)

-- | Represents the version rules for a Swift Package as defined in Xcode project file.
data XCRemoteSwiftPackageReference = XCRemoteSwiftPackageReference
  { -- | Represents repositoryURL field from project file.
    urlOf :: Text
  , -- | Represents requirement field from project file.
    requirementOf :: SwiftPackageGitDepRequirement
  }
  deriving (Show, Eq, Ord)

swiftPackageReferencesOf :: PbxProj -> [XCRemoteSwiftPackageReference]
swiftPackageReferencesOf pbx = mapMaybe toSwiftPkgRef swiftPkgRefObjects
  where
    swiftPkgRefObjects :: [Map Text AsciiValue]
    swiftPkgRefObjects = maybe [] (objectsFromIsa "XCRemoteSwiftPackageReference") (objects pbx)

    toSwiftPkgRef :: Map Text AsciiValue -> Maybe XCRemoteSwiftPackageReference
    toSwiftPkgRef candidate = case (repositoryURL candidate, requirement candidate) of
      (Just url, Just req) -> Just $ XCRemoteSwiftPackageReference url req
      (_, _) -> Nothing

    repositoryURL :: Map Text AsciiValue -> Maybe Text
    repositoryURL v = Map.lookup "repositoryURL" v >>= textOf

    requirement :: Map Text AsciiValue -> Maybe SwiftPackageGitDepRequirement
    requirement v = Map.lookup "requirement" v >>= toReferenceRequirement

    toReferenceRequirement :: AsciiValue -> Maybe SwiftPackageGitDepRequirement
    toReferenceRequirement value =
      asum
        [ upToNextMajor
        , upToNextMinor
        , versionRange
        , revision
        , exactVersion
        , branch
        ]
      where
        get = lookupText value
        kind = get "kind"

        upToNextMajor =
          if kind == Just "upToNextMajorVersion"
            then UpToNextMajor <$> get "minimumVersion"
            else Nothing
        upToNextMinor =
          if kind == Just "upToNextMinorVersion"
            then UpToNextMinor <$> get "minimumVersion"
            else Nothing
        versionRange =
          if kind == Just "versionRange"
            then ClosedInterval <$> ((,) <$> get "minimumVersion" <*> get "maximumVersion")
            else Nothing
        branch =
          if kind == Just "branch"
            then Branch <$> get "branch"
            else Nothing
        revision =
          if kind == Just "revision"
            then Revision <$> get "revision"
            else Nothing
        exactVersion =
          if kind == Just "exactVersion"
            then Exact <$> get "version"
            else Nothing

toDependency :: XCRemoteSwiftPackageReference -> Dependency
toDependency src =
  Dependency
    { dependencyType = depType
    , dependencyName = urlOf src
    , dependencyVersion = Just $ toConstraint $ requirementOf src
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = Map.empty
    }
  where
    depType :: DepType
    depType =
      if isGitRefConstraint $ requirementOf src
        then GitType
        else SwiftType

buildGraph :: PbxProj -> Maybe SwiftPackageResolvedFile -> Graphing.Graphing Dependency
buildGraph projFile maybeResolvedContent =
  case maybeResolvedContent of
    Nothing -> directs $ map toDependency $ swiftPackageReferencesOf projFile
    Just resolvedContent -> promoteToDirect isDirect $ deeps $ resolvedDependenciesOf resolvedContent
  where
    isDirect :: Dependency -> Bool
    isDirect dep = (dependencyName dep) `member` fromList (map urlOf $ swiftPackageReferencesOf projFile)

-- | Checks if XCode Project File has at-least one swift dependency.
-- It does by counting instances of `XCRemoteSwiftPackageReference` in the project file.
hasSomeSwiftDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m Bool
hasSomeSwiftDeps projFile = do
  xCodeProjContent <- readContentsParser parsePbxProj projFile
  pure $ (not . null) (swiftPackageReferencesOf xCodeProjContent)

analyzeXcodeProjForSwiftPkg :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> Maybe (Path Abs File) -> m (Graphing.Graphing Dependency)
analyzeXcodeProjForSwiftPkg xcodeProjFile resolvedFile = do
  xCodeProjContent <-
    context "Identifying swift package references in xcode project file" $
      readContentsParser parsePbxProj xcodeProjFile

  packageResolvedContent <- case resolvedFile of
    Nothing -> pure Nothing
    Just packageResolved ->
      context "Identifying dependencies in Package.resolved" $
        readContentsJson packageResolved

  context "Building dependency graph" $ pure $ buildGraph xCodeProjContent packageResolvedContent
