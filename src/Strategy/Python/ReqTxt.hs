
module Strategy.Python.ReqTxt
  ( discover
  , analyze
  )
  where

import Prologue

import Control.Carrier.Error.Either
import Control.Carrier.Writer.Strict
import Text.Megaparsec
import Text.Megaparsec.Char

import Diagnostics
import Discovery.Walk
import Effect.ReadFS
import Strategy.Python.Util
import Types

discover :: Discover
discover = Discover
  { discoverName = "requirements.txt"
  , discoverFunc = discover'
  }

discover' ::
  ( Has ReadFS sig m
  , Has (Writer [ProjectClosure]) sig m
  , MonadIO m
  , Effect sig
  )
  => Path Abs Dir -> m ()
discover' = walk $ \_ _ files -> do
  let txtFiles = filter (\f -> "req" `isPrefixOf` fileName f
                            && ".txt" `isSuffixOf` fileName f) files

  for_ txtFiles $ \file -> do
    res <- runError @ReadFSErr (analyze file)
    traverse_ (tell @[ProjectClosure] . pure) res

  walkContinue

analyze :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path Rel File -> m ProjectClosure
analyze file = mkProjectClosure file <$> readContentsParser requirementsTxtParser file

mkProjectClosure :: Path Rel File -> [Req] -> ProjectClosure
mkProjectClosure file reqs = ProjectClosure
  { closureStrategyGroup = PythonGroup
  , closureStrategyName  = "python-requirements"
  , closureModuleDir     = parent file
  , closureDependencies  = dependencies
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph reqs
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

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
