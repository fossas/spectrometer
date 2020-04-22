module VPSScan.RunSherlock ( scan, SherlockOpts(..) ) where
import Prologue

import Control.Carrier.Error.Either
import Effect.Exec
import qualified Data.Text as T

data SherlockOpts = SherlockOpts
  { sherlockCmdPath :: String
  , sherlockUrl :: String
  , sherlockClientToken :: String
  , sherlockClientID :: String
  } deriving (Eq, Ord, Show, Generic)

scan :: (Has Exec sig m, Has (Error ExecErr) sig m) => Path Abs Dir -> Text -> SherlockOpts -> m ()
scan baseDir scanId opts@SherlockOpts{..}  = do
  let c :: [String]
      c = [sherlockCmdPath]
      sherlockCommand :: Command
      sherlockCommand = Command c [] Never
  _ <- execThrow baseDir sherlockCommand $ sherlockCmdArgs (T.unpack scanId) opts
  pure ()

sherlockCmdArgs :: String -> SherlockOpts -> [String]
sherlockCmdArgs scanId SherlockOpts{..} = [ "--scan-id", scanId
                                          , "--sherlock-client-token", sherlockClientToken
                                          , "--sherlock-client-id", sherlockClientID
                                          ]
