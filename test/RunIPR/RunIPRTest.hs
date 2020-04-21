{-# LANGUAGE OverloadedStrings #-}

module RunIPR.RunIPRTest
  ( spec_run
  ) where

import Prologue

import VPSScan.RunIPR as RunIPR
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M


import Test.Tasty.Hspec

licenseOne :: IprLicense
licenseOne = RunIPR.IprLicense "Public-domain"

licenseExpressionOne :: IprLicenseExpression
licenseExpressionOne = RunIPR.IprLicenseExpression 0 "Public-domain" [licenseOne] Nothing "in this ebook edition are believed to be in the U.S. public domain." 552 619
licenseExpressionTwo :: IprLicenseExpression
licenseExpressionTwo = RunIPR.IprLicenseExpression 1 "Public-domain" [licenseOne] Nothing "is released under the terms in the CC0 1.0 Universal Public Domain" 639 705

fileOne :: IprFile
fileOne = RunIPR.IprFile "epub/content.opf" $ M.fromList [("0", licenseExpressionOne), ("1", licenseExpressionTwo)]
fileTwo :: IprFile
fileTwo = RunIPR.IprFile  "epub/css/core.css" M.empty
fileThree :: IprFile
fileThree = RunIPR.IprFile "epub/css/local.css" M.empty
fileFour :: IprFile
fileFour = RunIPR.IprFile "epub/text/chapter-1.xhtml" M.empty

basicResultFileList :: [IprFile]
basicResultFileList = [fileOne, fileTwo, fileThree, fileFour]

spec_run :: Spec
spec_run = do
  basicResult <- runIO (BL.readFile "test/RunIPR/testdata/result.json")
  describe "parseJSON" $ do
    it "should find a list of files" $ do
      case eitherDecode $ basicResult of
        Right result ->
          iprResponseFiles result `shouldBe` basicResultFileList
        Left err -> expectationFailure (T.unpack ("could not parse IPR result") <> err)

