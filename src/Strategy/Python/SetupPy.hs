module Strategy.Python.SetupPy
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
  { discoverName = "setup.py"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, ReadFS, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files ->
  case find (\f -> fileName f == "setup.py") files of
    Nothing -> walkContinue
    Just file -> do
      res <- runError @ReadFSErr (analyze file)
      traverse_ (output . dummyConfigure "python-setuppy" NotOptimal NotComplete (parent file)) res
      walkContinue

analyze :: Members '[ReadFS, Error ReadFSErr] r => Path Rel File -> Sem r (Graphing Dependency)
analyze file = do
  reqs <- readContentsParser installRequiresParser file
  pure (buildGraph reqs)

type Parser = Parsec Void Text

installRequiresParser :: Parser [Req]
installRequiresParser = prefix *> entries <* end
  where
  prefix  = skipManyTill anySingle (string "install_requires=[")
  entries = between quote quote requirementParser `sepBy` char ','
  end     = char ']'

  quote   = char '\''
