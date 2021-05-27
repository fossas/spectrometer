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
import Control.Effect.Diagnostics
import Text.Megaparsec
import Data.Void (Void)

data Resolver = Resolver
  { resolverName :: Text
  , resolverSupportsLocator :: Locator -> Bool
  , resolverLocatorToPackage :: Locator -> Either Text Package
  }

data Package
  = WorkspacePackage (Path Rel Dir)
  | NpmPackage (Maybe Text) Text Text -- scope, package, version
  | GitPackage Text Text -- url, commit
  | TarPackage Text -- url
  deriving (Eq, Ord, Show)

---------- WorkspaceResolver

workspaceProtocol :: Text
workspaceProtocol = "workspace:"

-- | Resolved workspace locators come in the form @workspace:./relative/path@
workspaceResolver :: Resolver
workspaceResolver =
  Resolver
    { resolverName = "WorkspaceResolver"
    , resolverSupportsLocator = (workspaceProtocol `T.isPrefixOf`) . locatorReference
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
    { resolverName = "NpmResolver"
    , resolverSupportsLocator = \loc ->
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
    { resolverName = "GitResolver"
    , resolverSupportsLocator = ("commit=" `T.isInfixOf`) . locatorReference
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

tag :: a -> Maybe b -> Either a b
tag a = maybe (Left a) Right

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

---------- TarResolver

type Parser = Parsec Void Text

matchParser :: Parser a -> Text -> Bool
matchParser p = either (const False) (const True) . runParser p ""

-- For a locator to be a valid tar, it must match both of these regexes:
--
-- @
--     export const TARBALL_REGEXP = /^[^?]*\.(?:tar\.gz|tgz)(?:\?.*)?$/;
--     export const PROTOCOL_REGEXP = /^https?:/;
-- @
tarMatchP :: Parser ()
tarMatchP = do
  _ <- chunk "https:" <|> chunk "http:"
  lookForExtension
  _ <- optional (single '?' *> takeRest)
  eof

  where
    lookForExtension = do
      _ <- takeWhile1P Nothing (\c -> c /= '?' && c /= '.')
      found <- optional $ chunk ".tar.gz" <|> chunk ".tgz"
      case found of
        Nothing -> anySingle *> lookForExtension
        Just _ -> pure ()


tarResolver :: Resolver
tarResolver =
  Resolver
    { resolverName = "TarResolver"
    , resolverSupportsLocator = matchParser tarMatchP . locatorReference
    , resolverLocatorToPackage = Right . TarPackage . locatorReference
    }

---------- TODO: file:
----------

allResolvers :: [Resolver]
allResolvers = [workspaceResolver, npmResolver, gitResolver, tarResolver]

-- TODO: turn this into real diagnostics error?
resolveLocatorToPackage :: Has Diagnostics sig m => Locator -> m Package
resolveLocatorToPackage locator = context ("Resolving locator " <> showT locator) $ do
  resolver <- fromMaybe @Text "Unsupported locator (no resolver found)" $
    find (`resolverSupportsLocator` locator) allResolvers

  context ("Running resolver: " <> resolverName resolver) . fromEither $
    resolverLocatorToPackage resolver locator
