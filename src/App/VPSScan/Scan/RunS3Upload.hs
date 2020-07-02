module App.VPSScan.Scan.RunS3Upload
  (execS3Upload
  )
where

import qualified Aws
import qualified Aws.S3 as S3
import Control.Monad.Trans.Resource
import Network.HTTP.Conduit (newManager, Manager, tlsManagerSettings, RequestBody(..))
import Path.IO (listDirRecur, isSymlink)
import Control.Monad.Except
import qualified Data.ByteString as S
import Control.Carrier.TaskPool
import Effect.Logger
import GHC.Conc.Sync (getNumCapabilities)

import System.IO
import Control.Applicative
import qualified Data.Text as T
import Control.Carrier.Trace.Printing
import App.VPSScan.Types
import Control.Effect.Diagnostics
import Prologue

data S3UploadError = ErrorRunningS3Upload Text
  deriving (Eq, Ord, Show, Generic, Typeable)

instance ToDiagnostic S3UploadError where
  renderDiagnostic = \case
    ErrorRunningS3Upload err -> "Error while uploading to S3: " <> pretty err

execS3Upload :: (Has Diagnostics sig m, MonadIO m, Has Trace sig m) => Path Abs Dir -> Text -> IPROpts -> m ()
execS3Upload basedir scanId IPROpts{..} = do
  cfg <- Aws.baseConfiguration
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

  mgr <- liftIO $ newManager tlsManagerSettings
  trace "[S3] Finding files to upload"
  -- listDirRecur returns a list of all of the files in a directory recursively, but doesn't recurse down symlinked dirs
  (_, allFilesAndSymlinks) <- listDirRecur basedir
  -- listDirRecur returns symlinked files, which we don't want, so get rid of them before uploading
  allFiles <- filterM (liftM not . isSymlink) allFilesAndSymlinks
  trace $ "[S3] " ++ (show $ length allFiles) ++ " files found. Starting upload to S3"

  capabilities <- liftIO getNumCapabilities
  trace $ "[S3] Uploading to S3 in " ++ show capabilities ++ " threads"

  _ <- liftIO $ withLogger SevTrace $ withTaskPool capabilities updateProgress $ traverse_ (uploadAbsFilePath cfg s3cfg mgr (T.pack s3Bucket) basedir scanId) allFiles
  pure ()

updateProgress :: Has Logger sig m => Progress -> m ()
updateProgress Progress{..} =
  logSticky ( "[ "
            <> annotate (color Cyan) (pretty pQueued)
            <> " Waiting / "
            <> annotate (color Yellow) (pretty pRunning)
            <> " Running / "
            <> annotate (color Green) (pretty pCompleted)
            <> " Completed"
            <> " ]" )

uploadAbsFilePath :: (Has TaskPool sig m, MonadIO m) => Aws.Configuration -> S3.S3Configuration Aws.NormalQuery -> Manager -> Text -> Path Abs Dir -> Text -> Path Abs File -> m ()
uploadAbsFilePath    cfg s3cfg mgr bucketName basedir scanId filepath =
  case stripProperPrefix basedir filepath of
    Nothing -> pure ()
    Just relPath -> do
      let key = scanId <> "/" <> (T.pack $ fromRelFile relPath)
      uploadFileToS3 cfg s3cfg mgr bucketName filepath key

uploadFileToS3 :: (Has TaskPool sig m, MonadIO m) => Aws.Configuration -> S3.S3Configuration Aws.NormalQuery -> Manager -> Text -> Path Abs File -> Text -> m ()
uploadFileToS3 cfg s3cfg mgr bucketName filePath key = do
  _ <- liftIO $ runResourceT $ do
    -- streams large file content, without buffering more than 10k in memory
    let streamer sink = withFile (fromAbsFile filePath) ReadMode $ \h -> sink $ S.hGet h 10240
    size <- liftIO $ withFile (fromAbsFile filePath) ReadMode hFileSize
    let body = RequestBodyStream (fromInteger size) streamer
    rsp <- Aws.pureAws cfg s3cfg mgr $
        (S3.putObject bucketName key body)
      { S3.poMetadata =
        [ ("mediatype", "texts")
        , ("meta-description", "test Internet Archive item made via haskell aws library")
        ]
      -- Automatically creates bucket on IA if it does not exist,
      -- and uses the above metadata as the bucket's metadata.
      , S3.poAutoMakeBucket = True
      }
    pure rsp
  pure ()