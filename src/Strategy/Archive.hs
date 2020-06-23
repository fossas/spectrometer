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
import Debug.Trace
import Control.Effect.Exception (SomeException, try)

-- FIXME: module paths include the tmp directory
-- Given a discover function to run over unarchived contents, discover projects
discover :: HasDiscover sig m => (Path Abs Dir -> m ()) -> Path Abs Dir -> m ()
discover go = walk $ \_ _ files -> do
  let tars = filter (\file -> ".tar" `isSuffixOf` fileName file) files
  traverse_ (\file -> withTar file go) tars

  let tarGzs = filter (\file -> ".tar.gz" `isSuffixOf` fileName file) files
  traverse_ (\file -> withTarGz file go) tarGzs
 
  pure WalkContinue

withTar :: Has (Lift IO) sig m => Path Abs File -> (Path Abs Dir -> m ()) -> m ()
withTar = withArchive (\dir file -> sendIO $ Tar.unpack (fromAbsDir dir) . removeTarLinks . Tar.read =<< BL.readFile (fromAbsFile file))

withTarGz :: Has (Lift IO) sig m => Path Abs File -> (Path Abs Dir -> m ()) -> m ()
withTarGz =
  withArchive
    ( \dir file ->
        sendIO $
          Tar.unpack (fromAbsDir dir) . removeTarLinks . Tar.read . GZip.decompress =<< BL.readFile (fromAbsFile file)
    )

-- The tar unpacker dies when tar files reference files outside of the archive root
removeTarLinks :: Tar.Entries e -> Tar.Entries e
removeTarLinks (Tar.Next x xs) =
  case Tar.entryContent x of
    Tar.HardLink _ -> xs
    Tar.SymbolicLink _ -> xs
    _ -> Tar.Next x (removeTarLinks xs)
removeTarLinks Tar.Done = Tar.Done
removeTarLinks (Tar.Fail e) = Tar.Fail e

-- FIXME: exceptions
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
  traceM (show file)
  res <- try @SomeException (extract tmpDir file)
  case res of
    Left err -> traceM (show err)
    Right _ -> go tmpDir
  traceM (show tmpDir)
  go tmpDir
