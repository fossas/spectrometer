module App.VPSScan.Scan.RunS3Upload
  (execS3Upload
  )
where

import App.VPSScan.Types
import Control.Carrier.Error.Either
import Control.Carrier.Trace.Printing
import Control.Effect.Diagnostics
import System.Process.Typed as PROC
import System.FilePath (joinPath)
import Data.Text.Prettyprint.Doc (pretty)
import qualified Data.Text as T
import Effect.Exec
import Prologue

data S3UploadError = ErrorRunningS3Upload Text
  deriving (Eq, Ord, Show, Generic, Typeable)

instance ToDiagnostic S3UploadError where
  renderDiagnostic = \case
    ErrorRunningS3Upload err -> "Error while uploading to S3: " <> pretty err

execS3Upload :: (Has Trace sig m, Has Diagnostics sig m, MonadIO m) => Path Abs Dir -> Text -> IPROpts -> m ()
execS3Upload basedir scanId IPROpts{..} = do
  trace $ "Uploading to S3 with this command: " ++ commandString
  (exitcode, stdout, stderr) <- PROC.readProcess (setWorkingDir (fromAbsDir basedir) (PROC.shell commandString))
  case (exitcode, stdout, stderr) of
    (ExitSuccess, out, _) -> do
      trace $ "stdout: " ++ show out
      pure ()
    (_, _, err) -> fatal (ErrorRunningS3Upload (T.pack (show err)))
  where
    s3Location = joinPath [s3Url, (T.unpack scanId)]
    cdPart = "cd " ++ fromAbsDir basedir
    uploadPart = " | sed 's/^\\.\\///' | shuf | xargs -n 1 -P 100 -I{} aws s3 cp {} " ++ s3Location ++ "/{} --quiet"
    findFilesPart = "{ find . -maxdepth 1 -mindepth 1 -type f; find . -maxdepth 2 -mindepth 2 -type f; find . -maxdepth 3 -mindepth 3 -type f; }"
    findDirsPart = "find . -maxdepth 3 -mindepth 3 -type d"
    commandString = cdPart ++ " && " ++ findFilesPart ++ uploadPart ++ " && " ++ findDirsPart ++ uploadPart ++ " --recursive"