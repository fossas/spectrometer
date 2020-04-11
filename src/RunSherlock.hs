module RunSherlock ( scan ) where
import Prologue

import Control.Carrier.Error.Either
import Effect.Exec

scan :: (Has Exec sig m, Has (Error ExecErr) sig m) => Path Abs Dir -> Maybe String -> Maybe String -> m ()
scan baseDir sherlockCmdPath sherlockApiKey = do
  case sequenceA [sherlockCmdPath, sherlockApiKey] of
    Nothing -> return ()
    Just [cmdPath, key] -> do
      let c :: [String]
          c = [cmdPath]
          sherlockCommand :: Command
          sherlockCommand = Command c [] Never
      _ <- execThrow baseDir sherlockCommand [key]
      pure ()
    _ -> return ()
