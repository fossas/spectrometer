
module Strategy.Python.ReqTxt
  ( discover
  , analyze
  )
  where

import Prologue

import Polysemy
import Polysemy.Error
import Polysemy.Output
import Text.Megaparsec
import Text.Megaparsec.Char

import Diagnostics
import Discovery.Walk
import DepTypes
import Graphing
import Effect.ReadFS
import Strategy.Python.Util
import Types

discover :: Discover
discover = Discover
  { discoverName = "requirements.txt"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, ReadFS, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files ->
  case find (\f -> fileName f == "requirements.txt") files of
    Nothing -> walkContinue
    Just file -> do
      res <- runError @ReadFSErr (analyze file)
      traverse_ (output . dummyConfigure "python-requirements" Optimal NotComplete (parent file)) res
      walkContinue

analyze :: Members '[ReadFS, Error ReadFSErr] r => Path Rel File -> Sem r (Graphing Dependency)
analyze file = do
  reqs <- readContentsParser requirementsTxtParser file
  pure (buildGraph reqs)

type Parser = Parsec Void Text

-- https://pip.pypa.io/en/stable/reference/pip_install/#requirements-file-format
requirementsTxtParser :: Parser [Req]
requirementsTxtParser = concat <$> ((line `sepBy` eol) <* eof)
  where
  isEndLine :: Char -> Bool
  isEndLine '\n' = True
  isEndLine '\r' = True
  isEndLine _    = False

  -- ignore content until the end of the line
  ignored :: Parser ()
  ignored = () <$ takeWhileP (Just "ignored") (not . isEndLine)

  comment :: Parser ()
  comment = char '#' *> ignored

  -- FUTURE: we can case split / sum-type this for better analysis
  line = [] <$ char '-' <* ignored -- pip options
     <|> [] <$ char '.' <* ignored -- relative path
     <|> [] <$ char '/' <* ignored -- absolute path
     <|> [] <$ oneOfS ["http:", "https:", "git+", "hg+", "svn+", "bzr+"] <* ignored -- URLs
     <|> [] <$ comment
     <|> (pure <$> requirementParser <* optional comment)
     <|> pure [] -- empty line

  oneOfS = asum . map string
