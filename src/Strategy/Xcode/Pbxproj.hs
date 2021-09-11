module Strategy.Xcode.Pbxproj (
  ) where

import Data.Foldable (asum)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Strategy.Xcode.PbxprojParser (AsciiValue (..), PbxProj (..), isaOf, lookupText, textOf)

data XCRemoteSwiftPackageReference = XCRemoteSwiftPackageReference
  { repositoryURL :: Text
  , requirement :: SwiftPackageReferenceRequirement
  }
  deriving (Show, Eq, Ord)

data SwiftPackageReferenceRequirement
  = UpToNextMajor Text
  | UpToNextMinor Text
  | VersionRange Text Text
  | Branch Text
  | Revision Text
  | ExactVersion Text
  deriving (Show, Eq, Ord)

toSwiftPackageReferences :: PbxProj -> [XCRemoteSwiftPackageReference]
toSwiftPackageReferences pbx = mapMaybe (transform) (fromMaybe [] swiftPkgRefObjects)
  where
    swiftPkgRefObjects :: Maybe [Map Text AsciiValue]
    swiftPkgRefObjects = isaOf "XCRemoteSwiftPackageReference" <$> objects pbx

    transform :: Map Text AsciiValue -> Maybe XCRemoteSwiftPackageReference
    transform candidate = case (repositoryURLOf candidate, requirementKindOf candidate) of
      (Just url, Just req) -> Just $ XCRemoteSwiftPackageReference url req
      (_, _) -> Nothing

    repositoryURLOf :: Map Text AsciiValue -> Maybe Text
    repositoryURLOf v = Map.lookup "repositoryURL" v >>= textOf

    requirementKindOf :: Map Text AsciiValue -> Maybe SwiftPackageReferenceRequirement
    requirementKindOf v = Map.lookup "requirement" v >>= toReferenceRequirement

toReferenceRequirement :: AsciiValue -> Maybe SwiftPackageReferenceRequirement
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
      if (kind == Just "upToNextMajorVersion")
        then UpToNextMajor <$> get "minimumVersion"
        else Nothing
    upToNextMinor =
      if (kind == Just "upToNextMinorVersion")
        then UpToNextMajor <$> get "minimumVersion"
        else Nothing
    versionRange =
      if (kind == Just "versionRange")
        then (VersionRange <$> get "minimumVersion" <*> get "maximumVersion")
        else Nothing
    branch =
      if (kind == Just "branch")
        then (Branch <$> get "branch")
        else Nothing
    revision =
      if (kind == Just "revision")
        then (Revision <$> get "revision")
        else Nothing
    exactVersion =
      if (kind == Just "exactVersion")
        then (ExactVersion <$> get "version")
        else Nothing
