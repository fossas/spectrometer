module App.Fossa.IAT.Types (
  Fingerprint (..),
  UserDefinedAssertionMeta (..),
  extractUserDefinedBinaries,
) where

import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  withObject,
  (.:),
 )
import Data.Text (Text)
import DepTypes (DepType (IATType), Dependency (dependencyType))

-- | Fingerprint uniquely idenfies a file, derived from its content.
-- Fingerprints are backed by base16 representations of underlying data.
newtype Fingerprint = Fingerprint {unFingerprint :: Text}

instance ToJSON Fingerprint where
  toJSON = toJSON . unFingerprint

-- | User provided data to assert a binary via IAT.
data UserDefinedAssertionMeta = UserDefinedAssertionMeta
  { assertedName :: Text
  , assertedVersion :: Text
  , assertedLicense :: Text
  , assertedDescription :: Maybe Text
  , assertedUrl :: Maybe Text
  }

instance FromJSON UserDefinedAssertionMeta where
  parseJSON = withObject "UserDefinedAssertionMetadata" $ \obj -> do
    UserDefinedAssertionMeta
      <$> obj .: "name"
      <*> obj .: "version"
      <*> obj .: "license"
      <*> obj .: "description"
      <*> obj .: "url"

-- | Extract user defined binary dependencies from a list of dependencies.
-- Evaluates to the list of user defined binaries and the remaining graph.
extractUserDefinedBinaries :: [Dependency] -> ([Dependency], [Dependency])
extractUserDefinedBinaries graph = (filter isIATDep graph, filter (not . isIATDep) graph)

isIATDep :: Dependency -> Bool
isIATDep d = (dependencyType d) == IATType
