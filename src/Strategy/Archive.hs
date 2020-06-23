module Strategy.Archive
  ( discover,
  )
where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Effect.Lift
import Control.Effect.Path (withSystemTempDir)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Discovery.Walk
import Data.List (isSuffixOf)
import Path
import Types
import Prelude

-- Given a discover function to run over unarchived contents, discover projects
discover :: HasDiscover sig m => (Path Abs Dir -> m ()) -> Path Abs Dir -> m ()
discover go = walk $ \_ _ files -> do
  let tars = filter (\file -> ".tar.gz" `isSuffixOf` fileName file) files
  traverse_ (\file -> withTar file go) tars

  let tarGzs = filter (\file -> ".tar.gz" `isSuffixOf` fileName file) files
  traverse_ (\file -> withTarGz file go) tarGzs
 
  pure WalkContinue

-- TODO: exceptions?
withTar :: Has (Lift IO) sig m => Path Abs File -> (Path Abs Dir -> m ()) -> m ()
withTar = withArchive (\dir file -> sendIO $ Tar.extract (fromAbsDir dir) (fromAbsFile file))

withTarGz :: Has (Lift IO) sig m => Path Abs File -> (Path Abs Dir -> m ()) -> m ()
withTarGz =
  withArchive
    ( \dir file ->
        sendIO $
          Tar.unpack (fromAbsDir dir) . Tar.read . GZip.decompress =<< BL.readFile (fromAbsFile file)
    )

withArchive ::
  Has (Lift IO) sig m =>
  -- | Archive extraction function
  (Path Abs Dir -> Path Abs File -> m ()) ->
  -- | Path to archive
  Path Abs File ->
  -- | Callback
  (Path Abs Dir -> m ()) ->
  m ()
withArchive extract file go = withSystemTempDir (fileName file) $ \tmpDir -> do
  extract tmpDir file
  go tmpDir
