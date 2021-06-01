-- TODO: figure out wth a virtual package is and if they can appear in locators
module Strategy.Yarn.Resolvers (
  Resolver (..),
  Package (..),
  resolveLocatorToPackage,
) where

import Control.Effect.Diagnostics
import Data.Either (isRight)
import Data.Foldable (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.SemVer qualified as SemVer
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Extra (dropPrefix, showT)
import Data.Void (Void)
import Strategy.Yarn.LockfileV2
import Text.Megaparsec

data Resolver = Resolver
  { resolverName :: Text
  , resolverSupportsLocator :: Locator -> Bool
  , resolverLocatorToPackage :: Locator -> Either Text Package
  }

data Package
  = WorkspacePackage Text -- relative reference to a directory. not quite a Path Rel Dir because it may contain '..'
  | NpmPackage (Maybe Text) Text Text -- scope, package, version
  | GitPackage Text Text -- url, commit
  | TarPackage Text -- url
  | FilePackage Text
  | LinkPackage Text
  | PortalPackage Text
  | ExecPackage Text
  | PatchPackage Text
  deriving (Eq, Ord, Show)

----------

resolveLocatorToPackage :: Has Diagnostics sig m => Locator -> m Package
resolveLocatorToPackage locator = context ("Resolving locator " <> showT locator) $ do
  resolver <-
    fromMaybe @Text "Unsupported locator (no resolver found)" $
      find (`resolverSupportsLocator` locator) allResolvers

  context ("Running resolver: " <> resolverName resolver) . fromEither $
    resolverLocatorToPackage resolver locator

allResolvers :: [Resolver]
allResolvers =
  [ workspaceResolver
  , npmResolver
  , gitResolver
  , tarResolver
  , fileResolver
  , linkResolver
  , execResolver
  , portalResolver
  , patchResolver
  ]

---------- WorkspaceResolver

workspaceProtocol :: Text
workspaceProtocol = "workspace:"

-- | Resolved workspace locators come in the form @workspace:./relative/reference/to/dir@
--
-- Relative references may contain '..', so they're not quite @Path Rel Dir@
workspaceResolver :: Resolver
workspaceResolver =
  Resolver
    { resolverName = "WorkspaceResolver"
    , resolverSupportsLocator = (workspaceProtocol `T.isPrefixOf`) . locatorReference
    , resolverLocatorToPackage =
        Right
          . WorkspacePackage
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
  (url, metadata) <-
    tag ("Invalid git reference: " <> locatorReference loc) $
      splitSingle "#" (locatorReference loc)

  metaMap <-
    tag ("Failed to parse git metadata: " <> metadata) $
      parseGitMetadata metadata

  commit <-
    tag ("Couldn't find commit in git metadata: " <> showT metaMap) $
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

-- | For a locator to be a valid tar, it must match both of these regexes:
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

-- | The tar resolver supports http/https URLs that point to a tarball (.tar.gz/.tgz)
tarResolver :: Resolver
tarResolver =
  Resolver
    { resolverName = "TarResolver"
    , resolverSupportsLocator = matchParser tarMatchP . locatorReference
    , resolverLocatorToPackage = Right . TarPackage . locatorReference
    }

---------- FileResolver

fileProtocol :: Text
fileProtocol = "file:"

-- | The file resolver supports local "file:" references on disk
--
-- Fossa cannot handle these, so we don't do any further parsing of the
-- resolution field
fileResolver :: Resolver
fileResolver =
  Resolver
    { resolverName = "FileResolver"
    , resolverSupportsLocator = (fileProtocol `T.isPrefixOf`) . locatorReference
    , resolverLocatorToPackage = Right . FilePackage . locatorReference
    }

---------- LinkResolver

linkProtocol :: Text
linkProtocol = "link:"

-- | The link resolver is similar to the file resolver
--
-- Fossa cannot handle these, so we don't do any further parsing of the
-- resolution field
linkResolver :: Resolver
linkResolver =
  Resolver
    { resolverName = "LinkResolver"
    , resolverSupportsLocator = (linkProtocol `T.isPrefixOf`) . locatorReference
    , resolverLocatorToPackage = Right . LinkPackage . locatorReference
    }

---------- PortalResolver

portalProtocol :: Text
portalProtocol = "portal:"

-- | The portal resolver is similar to the link resolver
--
-- Fossa cannot handle these, so we don't do any further parsing of the
-- resolution field
portalResolver :: Resolver
portalResolver =
  Resolver
    { resolverName = "PortalResolver"
    , resolverSupportsLocator = (portalProtocol `T.isPrefixOf`) . locatorReference
    , resolverLocatorToPackage = Right . PortalPackage . locatorReference
    }

---------- ExecResolver

execProtocol :: Text
execProtocol = "exec:"

-- | The exec resolver allows you to point to a script to run; the output of the
-- script is used as a package
--
-- Fossa cannot handle these, so we don't do any further parsing of the
-- resolution field
execResolver :: Resolver
execResolver =
  Resolver
    { resolverName = "ExecResolver"
    , resolverSupportsLocator = (execProtocol `T.isPrefixOf`) . locatorReference
    , resolverLocatorToPackage = Right . ExecPackage . locatorReference
    }

---------- PatchResolver

patchProtocol :: Text
patchProtocol = "patch:"

-- | The patch resolver allows you to modify another package with patch files.
-- The packages appear elsewhere in the lockfile, so we don't do any further
-- processing of the resolution field
patchResolver :: Resolver
patchResolver =
  Resolver
    { resolverName = "PatchResolver"
    , resolverSupportsLocator = (patchProtocol `T.isPrefixOf`) . locatorReference
    , resolverLocatorToPackage = Right . PatchPackage . locatorReference
    }
