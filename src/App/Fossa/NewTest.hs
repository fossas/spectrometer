module App.Fossa.NewTest (
testCommand) where

import Options.Applicative
import Data.Text (Text)
import App.Fossa.SubCommand

data TestConfig = TestConfig
  { timeout :: Int
  , apiKey :: Maybe Text
  } deriving (Show)

testMerge :: Applicative f => p1 -> p2 -> a -> f a
testMerge _ _ = pure

testCommand :: SubCommand TestConfig TestConfig
testCommand = SubCommand "test" testParse testMerge doTest

testParse :: Parser TestConfig
testParse = TestConfig
  <$> option auto  (long "timeout" <> value 60)
  <*> optional (strOption (long "api-key"))

doTest :: TestConfig -> IO ()
doTest = print