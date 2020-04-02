module Srclib.Types
  ( SourceUnit(..)
  , SourceUnitType(..)
  , SourceUnitBuild(..)
  , SourceUnitDependency(..)
  , Locator(..)
  ) where

import Prologue

data SourceUnit = SourceUnit
  { sourceUnitName :: Text -- TODO: does core use this anywhere? if not, let's just use the filepath again
  , sourceUnitType :: SourceUnitType
  , sourceUnitManifest :: Text -- ^ path to manifest file
  , sourceUnitBuild :: SourceUnitBuild
  } deriving (Eq, Ord, Show, Generic)

data SourceUnitType = SourceUnitTypeDummyCLI
  deriving (Eq, Ord, Show, Generic)

data SourceUnitBuild = SourceUnitBuild
  { buildArtifact :: Text -- ^ always "default"
  , buildSucceeded :: Bool -- ^ always true
  , buildImports :: [Locator]
  , buildDependencies :: [SourceUnitDependency]
  } deriving (Eq, Ord, Show, Generic)

data SourceUnitDependency = SourceUnitDependency
  { sourceDepLocator :: Locator
  , sourceDepImports :: [Locator] -- omitempty
  -- , sourceDepData :: Aeson.Value
  } deriving (Eq, Ord, Show, Generic)

data Locator = Locator
  { locatorFetcher :: Text
  , locatorProject :: Text
  , locatorRevision :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)
