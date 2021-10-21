module App.Fossa.NewTest (
  testCommand,
) where

import App.Fossa.SubCommand
import Data.Text (Text)
import Options.Applicative

data TestConfig = TestConfig
  { timeout :: Int
  , apiKey :: Maybe Text
  }
  deriving (Show)

testMerge :: p1 -> p2 -> a -> IO a
testMerge _ _ a = putStrLn "run pre-test" >> pure a

testCommand :: SubCommand TestConfig TestConfig
testCommand = SubCommand "test" mempty testParse testMerge doTest

testParse :: Parser TestConfig
testParse =
  TestConfig
    <$> option auto (long "timeout" <> value 60)
    <*> optional (strOption (long "api-key"))

doTest :: TestConfig -> IO ()
doTest = print