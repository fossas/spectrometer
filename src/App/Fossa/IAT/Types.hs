module App.Fossa.IAT.Types (
  Fingerprint (..),
  UserDefinedAssertionMeta (..),
) where

import Data.Aeson
import Data.Text

-- | Fingerprint uniquely idenfies a file, derived from its content.
-- Fingerprints are backed by base16 representations of underlying data.
newtype Fingerprint = Fingerprint Text

instance ToJSON Fingerprint where
  toJSON = toJSON . toText

toText :: Fingerprint -> Text
toText (Fingerprint x) = x

-- | User provided data to assert a binary via IAT.
data UserDefinedAssertionMeta = UserDefinedAssertionMeta
  { assertedName :: Text
  , assertedVersion :: Text
  , assertedLicense :: Text
  , assertedDescription :: Maybe Text
  , assertedUrl :: Maybe Text
  }
