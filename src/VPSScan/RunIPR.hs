{-# LANGUAGE OverloadedStrings #-}

module VPSScan.RunIPR
  ( IPROpts(..)

  , IPR(..)
  , IPRC(..)
  , execIPR
  , IPRError(..)
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import Prologue

import Control.Carrier.Error.Either
import Effect.Exec

data IPROpts = IPROpts
  { iprCmdPath :: String
  , nomosCmdPath :: String
  , pathfinderCmdPath :: String
  } deriving (Eq, Ord, Show, Generic)

iprCmdArgs :: IPROpts -> [String]
iprCmdArgs IPROpts{..} = ["-nomossa", nomosCmdPath, "-pathfinder", pathfinderCmdPath]

extractNonEmptyFiles :: Value -> Maybe Array
extractNonEmptyFiles (Object obj) = do
  files <- HM.lookup "Files" obj
  filesAsArray <- case files of
    Array filesArray -> Just filesArray
    _ -> Nothing

  let filtered = V.filter hasLicenses filesAsArray

      hasLicenses :: Value -> Bool
      hasLicenses (Object file) =
        case HM.lookup "LicenseExpressions" file of
          Just (Object expressions) -> not (HM.null expressions)
          _ -> False
      hasLicenses _ = False

  pure filtered
extractNonEmptyFiles _ = Nothing

----- ipr effect

data IPRError
  = NoFilesEntryInOutput
  | IPRCommandFailed Text
  deriving (Eq, Ord, Show, Generic)

data IPR m k
  = ExecIPR (Path Abs Dir) IPROpts (Either IPRError Array -> m k)
  deriving Generic1

instance HFunctor IPR
instance Effect IPR

execIPR :: Has IPR sig m => Path Abs Dir -> IPROpts -> m (Either IPRError Array)
execIPR basedir iprOpts = send (ExecIPR basedir iprOpts pure)

----- production ipr interpreter
 
newtype IPRC m a = IPRC { runIPR :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m, Effect sig) => Algebra (IPR :+: sig) (IPRC m) where
  alg (R other) = IPRC (alg (handleCoercible other))
  alg (L (ExecIPR basedir opts@IPROpts{..} k)) = (k =<<) . IPRC $ do
    let iprCommand :: Command
        iprCommand = Command [iprCmdPath] [] Never

    maybeValue <- runError @ExecErr $ runExecIO $ execJson basedir iprCommand $ iprCmdArgs opts
    case maybeValue of
      Left err -> pure (Left (IPRCommandFailed (T.pack (show err))))
      Right value -> do
        let maybeExtracted = extractNonEmptyFiles value
        case maybeExtracted of
          Nothing -> pure (Left NoFilesEntryInOutput)
          Just extracted -> pure (Right extracted)
