module App.VPSScan.Scan.RunS3Upload
  (execS3Upload
  )
where

import App.VPSScan.Types
import Control.Carrier.Error.Either
import Control.Effect.Diagnostics
import qualified Data.Text as T
import Effect.Exec
import Prologue

execS3Upload :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> Text -> IPROpts -> m ()
execS3Upload basedir scanId iprOpts = void $ execThrow basedir (s3Command basedir scanId iprOpts)

s3Command :: Path Abs Dir -> Text -> IPROpts -> Command
s3Command basedir scanId IPROpts {..} =
  Command
    { cmdName = "aws"
    , cmdArgs = [ "s3", "sync", fromAbsDir basedir, s3Location ]
    , cmdAllowErr = Never
    }
  where
    s3Location = s3Url ++ "/" ++ (T.unpack scanId)
