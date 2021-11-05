module Strategy.Python.ReqTxt (
  analyze',
  requirementsTxtParser,
) where

import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Control.Monad (void)
import Data.Foldable (asum)
import Data.Text (Text)
import Data.Void (Void)
import DepTypes (Dependency)
import Effect.ReadFS (ReadFS, readContentsParser)
import Graphing (Graphing)
import Path (Abs, File, Path)
import Strategy.Python.Util (Req, buildGraph, requirementParser)
import Text.Megaparsec (
  MonadParsec (eof, takeWhileP, try),
  Parsec,
  manyTill,
  (<|>),
 )
import Text.Megaparsec.Char (char, string)

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze' file = do
  reqs <- readContentsParser requirementsTxtParser file
  context "Building dependency graph" $ pure (buildGraph reqs)

type Parser = Parsec Void Text

-- https://pip.pypa.io/en/stable/reference/pip_install/#requirements-file-format
requirementsTxtParser :: Parser [Req]
requirementsTxtParser = concat <$> manyTill reqParser eof

reqParser :: Parser [Req]
reqParser =
  [] <$ char '-' <* ignored -- pip options
    <|> [] <$ char '.' <* ignored -- relative path
    <|> [] <$ char '/' <* ignored -- absolute path
    <|> [] <$ oneOfS ["http:", "https:", "git+", "hg+", "svn+", "bzr+"] <* ignored -- URLs
    <|> [] <$ comment
    <|> (pure <$> try requirementParser <* ignored)
    <|> [] <$ ignored -- something else we're not able to parse
  where
    isEndLine :: Char -> Bool
    isEndLine '\n' = True
    isEndLine '\r' = True
    isEndLine _ = False

    -- ignore content until the end of the line
    ignored :: Parser ()
    ignored = void $ takeWhileP (Just "ignored") (not . isEndLine) <* takeWhileP (Just "end of line") isEndLine

    comment :: Parser ()
    comment = char '#' *> ignored

    oneOfS = asum . map string
