{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.IAT.Types (
  Fingerprint (..),
  Raw,
) where

import Data.Aeson
import Data.Text
import Data.Typeable

-- | Fingerprint uniquely idenfies a file, derived from its content.
-- Fingerprints are backed by base16 representations of underlying data.
newtype Fingerprint t = Fingerprint Text

instance ToJSON (Fingerprint t) where
  toJSON = toJSON . toText

toText :: Fingerprint t -> Text
toText (Fingerprint x) = x

-- | Raw describes a fingerprint calculated from the full unmodified content of a file.
-- Raw fingerprints are implemented as SHA256 hashes.
data Raw deriving (Typeable)
