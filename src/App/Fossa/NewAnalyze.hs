module App.Fossa.NewAnalyze (
  analyzeCommand,
) where

import App.Fossa.SubCommand
import Data.Flag
import Data.Text (Text)
import Options.Applicative

-- API key
-- --output
-- BaseDir
-- branch

data NoUpload = NoUpload

data AnalyzePreConfig = AnalyzePreConfig
  { analyzeBranch :: Maybe Text
  , outputMode :: Flag NoUpload
  }
  deriving (Show)

newtype AnalyzeConfig = AnalyzeConfig AnalyzePreConfig deriving (Show)

analyzeCommand :: SubCommand AnalyzePreConfig AnalyzeConfig
analyzeCommand = SubCommand "analyze" mempty analyzeParse analyzeMerge analyze

analyzeParse :: Parser AnalyzePreConfig
analyzeParse = AnalyzePreConfig
    <$> optional (strOption (long "branch" <> help "hfosicsaduibcesjbfew"))
    <*> flagOpt NoUpload (long "output" <> help "-----------------555--------------")

-- We can do things like setting up scan destination, validating basedir,
-- checking VCS values like branch and revision, etc.
analyzeMerge :: ConfigFile -> Envs -> AnalyzePreConfig -> IO AnalyzeConfig
analyzeMerge _ _ a = putStrLn "run pre-analyze" >> pure (AnalyzeConfig a)

analyze :: AnalyzeConfig -> IO ()
analyze = print
