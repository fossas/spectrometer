{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Strategy.Python.Setuptools
  ( discover
  ) where

import Data.List (isInfixOf, isSuffixOf)
import Control.Carrier.Output.IO
import Control.Effect.Diagnostics
import Control.Effect.Lift
import Control.Monad.IO.Class
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing)
import Path
import qualified Strategy.Python.ReqTxt as ReqTxt
import qualified Strategy.Python.SetupPy as SetupPy
import Types
import Data.Foldable (find)

discover ::
  ( MonadIO m,
    Has (Lift IO) sig m,
    Has ReadFS sig m,
    Has Diagnostics sig m
  ) =>
  Path Abs Dir -> m [NewProject m]
discover = walk' $ \dir _ files -> do
  let reqTxtFiles = filter (\f -> "req" `isInfixOf` fileName f
                            && ".txt" `isSuffixOf` fileName f) files

  let setupPyFile = find (\f -> fileName f == "setup.py") files

  let project = SetuptoolsProject
        { setuptoolsReqTxt = undefined
        , setuptoolsSetupPy = undefined
        , setuptoolsDir = dir
        }

  case (reqTxtFiles, setupPyFile) of
    ([], Nothing) -> pure ([], WalkContinue)
    _ -> pure ([mkProject project], WalkContinue)

-- >>> words "foo bar"

analyzeReqTxts :: (Has ReadFS sig m, Has Diagnostics sig m) => SetuptoolsProject -> m (Graphing Dependency)
analyzeReqTxts = fmap mconcat . traverse ReqTxt.analyze' . setuptoolsReqTxt

analyzeSetupPy :: (Has ReadFS sig m, Has Diagnostics sig m) => SetuptoolsProject -> m (Graphing Dependency)
analyzeSetupPy = maybe (pure mempty) SetupPy.analyze' . setuptoolsSetupPy

data SetuptoolsProject = SetuptoolsProject
  { setuptoolsReqTxt :: [Path Abs File]
  , setuptoolsSetupPy :: Maybe (Path Abs File)
  , setuptoolsDir :: Path Abs Dir
  }

mkProject :: (Has ReadFS sig m, Has Diagnostics sig m) => SetuptoolsProject -> NewProject m
mkProject project = NewProject
  { projectType = "python-setuptools"
  , projectDependencyGraph = (<>) <$> analyzeReqTxts project <*> analyzeSetupPy project
  , projectPath = setuptoolsDir project
  , projectLicenses = pure []
  }
