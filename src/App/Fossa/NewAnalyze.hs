module App.Fossa.NewAnalyze (
  analyzeCommand,
) where

import Data.Flag
import Data.Text (Text)
import Options.Applicative
import App.Fossa.SubCommand

-- API key
-- --output
-- BaseDir
-- branch

data NoUpload = NoUpload

data AnalyzeConfig = AnalyzeConfig
  { analyzeBranch :: Maybe Text
  , outputMode :: Flag NoUpload
  }
  deriving (Show)

analyzeCommand :: SubCommand AnalyzeConfig AnalyzeConfig
analyzeCommand = SubCommand "analyze" analyzeParse analyzeMerge analyze

analyzeParse :: Parser AnalyzeConfig
analyzeParse = AnalyzeConfig
  <$> optional (strOption (long "branch" <> help "hfosicsaduibcesjbfew"))
  <*> flagOpt NoUpload (long "output" <> help "-----------------555--------------")

analyzeMerge :: Applicative f => p1 -> p2 -> a -> f a
analyzeMerge _ _ = pure

analyze :: AnalyzeConfig -> IO ()
analyze = print
