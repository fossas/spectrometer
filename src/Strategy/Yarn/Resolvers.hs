-- TODO: figure out wth a virtual package is and if they can appear in locators
module Strategy.Yarn.Resolvers (
  Resolver (..),
  Package (..),
  resolveLocatorToPackage,
) where

import Data.Either (isRight)
import Data.Foldable (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.SemVer qualified as SemVer
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Extra (dropPrefix, showT)
import Path
import Strategy.Yarn.LockfileV2
import Data.Bifunctor (first)

data Resolver = Resolver
  { resolverSupportsLocator :: Locator -> Bool
  , resolverLocatorToPackage :: Locator -> Either Text Package
  }

data Package
  = WorkspacePackage (Path Rel Dir)
  | NpmPackage (Maybe Text) Text Text -- scope, package, version
  | GitPackage Text Text -- url, commit
  deriving (Eq, Ord, Show)

---------- WorkspaceResolver

workspaceProtocol :: Text
workspaceProtocol = "workspace:"

-- | Resolved workspace locators come in the form @workspace:./relative/path@
workspaceResolver :: Resolver
workspaceResolver =
  Resolver
    { resolverSupportsLocator = (workspaceProtocol `T.isPrefixOf`) . locatorReference
    , resolverLocatorToPackage =
        fmap WorkspacePackage
          . first showT
          . parseRelDir
          . T.unpack
          . dropPrefix workspaceProtocol
          . locatorReference
    }

---------- NpmResolver

npmProtocol :: Text
npmProtocol = "npm:"

npmResolver :: Resolver
npmResolver =
  Resolver
    { resolverSupportsLocator = \loc ->
        (npmProtocol `T.isPrefixOf` locatorReference loc)
          || isValidSemver (locatorReference loc)
    , resolverLocatorToPackage = \loc ->
        Right $ NpmPackage (locatorScope loc) (locatorName loc) (dropPrefix npmProtocol (locatorReference loc))
    }

isValidSemver :: Text -> Bool
isValidSemver = isRight . SemVer.fromText

---------- GitResolver

-- | The git resolver in yarn ALWAYS normalizes and resolves git references the same way:
--
-- @
--     $URL#metadata
--
--     -- e.g.
--
--     https://github.com/foo/bar.git#commit=$COMMITID
--     https://example.com/baz.git#branch=$BRANCH&commit=$COMMITID
-- @
--
-- The string after # is a set of key/value pairs, separated by &
--
-- We can always expect to find a commit key, so we use that for the package
gitResolver :: Resolver
gitResolver =
  Resolver
    { resolverSupportsLocator = ("commit=" `T.isInfixOf`) . locatorReference
    , resolverLocatorToPackage = gitResolverLocatorToPackage
    }

gitResolverLocatorToPackage :: Locator -> Either Text Package
gitResolverLocatorToPackage loc = do
  (url, metadata) <- tag ("Invalid git reference: " <> locatorReference loc) $
    splitSingle "#" (locatorReference loc)

  metaMap <- tag ("Failed to parse git metadata: " <> metadata) $
    parseGitMetadata metadata

  commit <- tag ("Couldn't find commit in git metadata: " <> showT metaMap) $
    M.lookup "commit" metaMap

  Right $ GitPackage url commit

-- | T.splitOn, but only expects to split once
splitSingle :: Text -> Text -> Maybe (Text, Text)
splitSingle needle txt =
  case T.splitOn needle txt of
    [a, b] -> pure (a, b)
    _ -> Nothing

-- | Parse a metadata string from a git yarn locator into a Map
--
-- The metadata string is formatted as "key1=foo&key2=bar&key3=baz"
parseGitMetadata :: Text -> Maybe (Map Text Text)
parseGitMetadata = fmap M.fromList . traverse (splitSingle "=") . T.splitOn "&"

---------- TODO: file: and tarball
----------

allResolvers :: [Resolver]
allResolvers = [workspaceResolver, npmResolver, gitResolver]

-- TODO: turn this into real diagnostics error?
resolveLocatorToPackage :: Locator -> Either Text Package
resolveLocatorToPackage locator = do
  case find (`resolverSupportsLocator` locator) allResolvers of
    Nothing -> Left $ "Couldn't find resolver for locator: " <> showT locator
    Just resolver ->
      case resolverLocatorToPackage resolver locator of
        Left err -> Left $ "Resolver failed when turning locator into package: " <> showT locator <> " : " <> err
        Right resolved -> Right resolved

tag :: a -> Maybe b -> Either a b
tag a = maybe (Left a) Right
