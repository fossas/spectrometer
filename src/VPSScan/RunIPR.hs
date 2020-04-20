{-# LANGUAGE OverloadedStrings #-}

module VPSScan.RunIPR ( scan, IprOpts, IprResponse(..), IprFile(..) ) where
import Prologue

import Control.Carrier.Error.Either
import Effect.Exec

data IprOpts = IprOpts
  { baseDir :: Path Abs Dir
  , iprCmdPath :: String
  , nomosCmdPath :: String
  , pathfinderCmdPath :: String
  } deriving (Eq, Ord, Show, Generic)

scan :: (Has Exec sig m, Has (Error ExecErr) sig m) => IprOpts -> m IprResponse
scan IprOpts{..} = do
  let c :: [String]
      c = [iprCmdPath]
      iprCommand :: Command
      iprCommand = Command c [] Never
  execJson baseDir iprCommand [nomosCmdPath, pathfinderCmdPath]

-- mkFileList :: IprResponse -> IprResponse
-- mkFileList iprOutput = IprResponse iprOutput

data IprResponse = IprResponse { iprResponseFiles :: [IprFile] } deriving (Eq, Ord, Show, Generic)

data IprFile = IprFile 
  { iprFilePath :: String
  -- , iprFileLicenseExpressions :: [IprLicenseExpression]
  -- , iprFileCopyrights :: [IprCopyright]
  -- , iprFileAssociations :: [IprAssociation]
  } deriving (Eq, Ord, Show, Generic)

data IprLicenseExpression = IprLicenseExpression
  { iprLicenseExpressionInstanceID :: Int
  , iprLicenseExpressionExpression :: String
  , iprLicenseExpressionLicenses :: [IprLicense]
  , iprLicenseExpressionDeclaration :: String
  , iprLicenseExpressionVerbatim :: String
  , iprLicenseExpressionOffsetStart :: Int
  , iprLicenseExpresseionOffsetEnd :: Int
  } deriving (Eq, Ord, Show, Generic)

data IprLicense = IprLicense
  { iprLicenseId :: String 
  , iprLicenseNotes :: String
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON IprResponse where
  parseJSON = withObject "IprResponse" $ \obj ->
    IprResponse <$> obj .: "Files"

instance FromJSON IprFile where
  parseJSON = withObject "IprFile" $ \obj ->
    IprFile <$> obj .: "Path"
            -- <*> obj .: "LicenseExpressions"

-- instance FromJSON IprLicenseExpression where
--   parseJSON = withObject "IprLicenseExpression" $ \obj ->
--     IprLicenseExpression <$> 