
module RunIPR.RunIPRTest
  ( spec_run
  ) where

import Prologue

import VPSScan.RunIPR as RunIPR
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

import Test.Tasty.Hspec

fileOne :: IprFile
fileOne = RunIPR.IprFile "epub/content.opf"
fileTwo :: IprFile
fileTwo = RunIPR.IprFile  "epub/css/core.css"
fileThree :: IprFile
fileThree = RunIPR.IprFile "epub/css/local.css"
fileFour :: IprFile
fileFour = RunIPR.IprFile "epub/text/chapter-1.xhtml"

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
        Left _ -> expectationFailure (T.unpack ("could not parse IPR result"))

