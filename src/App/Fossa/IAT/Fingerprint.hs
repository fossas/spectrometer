{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module App.Fossa.IAT.Fingerprint (
  Fingerprint (..),
  Raw,
  fingerprintContentsRaw,
) where

import Control.Carrier.Diagnostics
import Control.Effect.Lift
import Crypto.Hash
import Data.ByteString.Lazy qualified as BL
import Data.Data
import Data.String.Conversion
import Data.Text
import Discovery.Walk
import Effect.ReadFS
import Path

-- | Fingerprint uniquely idenfies a file, derived from its content.
-- Fingerprints are backed by base16 representations of underlying data.
newtype Fingerprint t = Fingerprint Text

-- | Raw describes a fingerprint calculated from the full unmodified content of a file.
-- Raw fingerprints are implemented as SHA256 hashes.
data Raw deriving (Typeable)

sha256 :: BL.ByteString -> Digest SHA256
sha256 = hashlazy

hashFile :: (Has (Lift IO) sig m) => FilePath -> m Text
hashFile file = do
  content <- sendIO $ BL.readFile file
  pure . toText . show $ sha256 content

fingerprintRaw :: (Has (Lift IO) sig m) => Path Abs File -> m (Fingerprint Raw)
fingerprintRaw file = do
  fp <- hashFile (toFilePath file)
  pure (Fingerprint fp)

fingerprintRawMany :: (Has (Lift IO) sig m) => [Path Abs File] -> m [Fingerprint Raw]
fingerprintRawMany [] = pure []
fingerprintRawMany (x : xs) = do
  fp <- fingerprintRaw x
  fps <- fingerprintRawMany xs
  pure (fp : fps)

fingerprintContentsRaw :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Lift IO) sig m) => Path Abs Dir -> m [Fingerprint Raw]
fingerprintContentsRaw = walk' $ \_ _ files -> do
  fps <- fingerprintRawMany files
  pure (fps, WalkContinue)
