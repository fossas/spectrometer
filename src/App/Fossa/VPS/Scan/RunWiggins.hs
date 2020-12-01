{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.Scan.RunWiggins
  ( execWiggins
  , WigginsOpts(..)
  )
where

import App.Fossa.VPS.Types
import App.Fossa.VPS.Scan.Core
import App.Fossa.VPS.EmbeddedBinary
import Control.Carrier.Error.Either
import Control.Effect.Diagnostics
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as T
import Effect.Exec
import Path

data WigginsOpts = WigginsOpts
  { scanDir :: Path Abs Dir
  , spectrometerArgs :: [Text]
  }

execWiggins :: (Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m ()
execWiggins binaryPaths opts = void $ execThrow (scanDir opts) (wigginsCommand binaryPaths opts)

wigginsCommand :: BinaryPaths -> WigginsOpts -> Command
wigginsCommand BinaryPaths{..} WigginsOpts{..} = do
  Command
    { cmdName = T.pack $ fromAbsFile wigginsBinaryPath,
      cmdArgs = spectrometerArgs,
      cmdAllowErr = Never
    }
