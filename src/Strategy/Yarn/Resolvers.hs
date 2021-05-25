-- TODO: figure out wth a virtual package is and if they can appear in locators
module Strategy.Yarn.Resolvers (
  Resolver (..),
  Package (..),
) where

import Data.Text (Text)
import Data.Text qualified as T
import Path
import Strategy.Yarn.LockfileV2
import Data.Text.Extra (dropPrefix)

data Resolver = Resolver
  { resolverSupportsLocator :: Locator -> Bool
  , -- FIXME: more error information?
    resolverLocatorToPackage :: Locator -> Maybe Package
  }

data Package
  = WorkspacePackage (Path Rel Dir)
  | NpmPackage (Maybe Text) Text Text -- scope, package, version

-- TODO: logic for adding the default protocol prefix to descriptors

---------- WorkspaceResolver

workspaceProtocol :: Text
workspaceProtocol = "workspace:"

-- FIXME: are resolved workspaces always relative directories? the other option in descriptors is to add a semver
workspaceResolver :: Resolver
workspaceResolver =
  Resolver
    { resolverSupportsLocator = (workspaceProtocol `T.isPrefixOf`) . locatorReference
    , resolverLocatorToPackage =
        fmap WorkspacePackage
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
    { resolverSupportsLocator = (npmProtocol `T.isPrefixOf`) . locatorReference
    , resolverLocatorToPackage = \loc ->
        Just $ NpmPackage (locatorScope loc) (locatorName loc) (dropPrefix npmProtocol (locatorReference loc))
    }

----------
