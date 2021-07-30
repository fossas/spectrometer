module Maven.DepTreeSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Strategy.Maven.DepTree
import Data.Text.IO qualified as TextIO
import Text.Megaparsec (parse)

spec :: Spec
spec =
  describe "dotfile parser" $ do
    dotfile <- runIO $ TextIO.readFile "test/Maven/testdata/fossa-deptree.dot"
    it "should parse files" $
      parse parseDotGraph "" `shouldSucceedOn` dotfile
