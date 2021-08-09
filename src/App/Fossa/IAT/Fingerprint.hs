module App.Fossa.IAT.Fingerprint (
  fingerprintContentsRaw,
) where

import App.Fossa.IAT.Types
import Conduit
import Control.Carrier.Diagnostics
import Control.Effect.Exception
import Control.Effect.Lift
import Crypto.Hash
import Data.ByteString qualified as B
import Data.String.Conversion
import Discovery.Walk
import Effect.ReadFS
import Path

-- | Hashes a stream of 'B.ByteString'@s@ and creates a digest @d@.
-- Adapted from @sinkHash@ in https://hackage.haskell.org/package/cryptonite-conduit-0.2.2/docs/src/Crypto-Hash-Conduit.html
sinkHash :: (Monad m, HashAlgorithm hash) => ConduitT B.ByteString Void m (Digest hash)
sinkHash = sink hashInit
  where
    sink ctx = do
      b <- await
      case b of
        Nothing -> return $! hashFinalize ctx
        Just bs -> sink $! hashUpdate ctx bs

-- | Hashes the whole contents of the given file in constant memory.
-- Adapted from @hashFile@ in https://hackage.haskell.org/package/cryptonite-conduit-0.2.2/docs/src/Crypto-Hash-Conduit.html
hashFile :: (Has (Lift IO) sig m, Has Diagnostics sig m, HashAlgorithm hash) => FilePath -> m (Digest hash)
hashFile fp =
  sendIO (runConduitRes (sourceFile fp .| sinkHash))
    `catch` (\(e :: IOException) -> fatalText ("unable to hash file: " <> toText (show e)))

fingerprintRaw :: (Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs File -> m Fingerprint
fingerprintRaw file = do
  fp <- hashFile' $ toFilePath file
  let base16 = toText . show $ fp
  pure (Fingerprint base16)
  where
    -- Monomorphize to SHA256 hash
    hashFile' :: (Has (Lift IO) sig m, Has Diagnostics sig m) => FilePath -> m (Digest SHA256)
    hashFile' = hashFile

fingerprintRawMany :: (Has (Lift IO) sig m, Has Diagnostics sig m) => [Path Abs File] -> m [Fingerprint]
fingerprintRawMany = traverse fingerprintRaw

fingerprintContentsRaw :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Lift IO) sig m) => Path Abs Dir -> m [Fingerprint]
fingerprintContentsRaw = walk' $ \_ _ files -> do
  fps <- fingerprintRawMany files
  pure (fps, WalkContinue)
