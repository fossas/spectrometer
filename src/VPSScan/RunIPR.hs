{-# LANGUAGE OverloadedStrings #-}

module VPSScan.RunIPR ( scan, IPROpts(..), IprResponse(..), IprFile(..), IprLicenseExpression(..), IprLicense(..) ) where
import Prologue

import Control.Carrier.Error.Either
import Effect.Exec

data IPROpts = IPROpts
  { iprCmdPath :: String
  , nomosCmdPath :: String
  , pathfinderCmdPath :: String
  } deriving (Eq, Ord, Show, Generic)

scan :: (Has Exec sig m, Has (Error ExecErr) sig m) => Path Abs Dir -> IPROpts -> m IprResponse
scan baseDir IPROpts{..} = do
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
  , iprFileLicenseExpressions :: Map String IprLicenseExpression
  -- , iprFileCopyrights :: [IprCopyright]
  -- , iprFileAssociations :: [IprAssociation]
  } deriving (Eq, Ord, Show, Generic)

data IprLicenseExpression = IprLicenseExpression
  { iprLicenseExpressionInstanceID :: Int
  , iprLicenseExpressionExpression :: String
  , iprLicenseExpressionLicenses :: [IprLicense]
  , iprLicenseExpressionDeclaration :: Maybe String
  , iprLicenseExpressionVerbatim :: String
  , iprLicenseExpressionOffsetStart :: Int
  , iprLicenseExpresseionOffsetEnd :: Int
  } deriving (Eq, Ord, Show, Generic)

data IprLicense = IprLicense
  { iprLicenseId :: String 
  -- , iprLicenseNotes :: String
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON IprResponse where
  parseJSON = withObject "IprResponse" $ \obj ->
    IprResponse <$> obj .: "Files"

instance FromJSON IprFile where
  parseJSON = withObject "IprFile" $ \obj ->
    IprFile <$> obj .: "Path"
            <*> obj .: "LicenseExpressions"

-- in this case, obj is a HashMap with a key we don't care about
-- and a value that contains the data for license expressiosn
instance FromJSON IprLicenseExpression where
  parseJSON = withObject "IprLicenseExpression" $ \obj ->
      IprLicenseExpression <$> obj .: "LicenseExpressionInstanceID"
                           <*> obj .: "Expression"
                           <*> obj .: "Licenses"
                           <*> obj .: "Declaration"
                           <*> obj .: "Verbatim"
                           <*> obj .: "OffsetStart"
                           <*> obj .: "OffsetEnd"

instance FromJSON IprLicense where
  parseJSON = withObject "IprLicense" $ \obj ->
    IprLicense <$> obj .: "ID"
              --  <*> obj .: "Notes"