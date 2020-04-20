module VPSScan.RunSherlock ( scan ) where
import Prologue

import Control.Carrier.Error.Either
import Effect.Exec

scan :: (Has Exec sig m, Has (Error ExecErr) sig m) => Path Abs Dir -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> String -> m ()
scan baseDir sherlockCmdPath sherlockApiKey sherlockUrl sherlockClientToken sherlockSecret scanId = do
  case sequenceA [sherlockCmdPath, sherlockApiKey, sherlockUrl, sherlockClientToken, sherlockSecret] of
    Nothing -> return ()
    Just [cmdPath, key, url, clientToken, secret] -> do
      let c :: [String]
          c = [cmdPath]
          sherlockCommand :: Command
          sherlockCommand = Command c [] Never
      _ <- execThrow baseDir sherlockCommand [key, scanId, url, clientToken, secret]
      pure ()
    _ -> return ()
