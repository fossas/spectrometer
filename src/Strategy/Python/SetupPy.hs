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
import Effect.ReadFS
import Strategy.Python.Util
import Types

discover :: Discover
discover = Discover
  { discoverName = "setup.py"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, ReadFS, Output ProjectClosure] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ _ files -> do
  case find (\f -> fileName f == "setup.py") files of
    Nothing -> pure ()
    Just file -> do
      res <- runError @ReadFSErr (analyze file)
      traverse_ output res
      pure ()

  walkContinue

analyze :: Members '[ReadFS, Error ReadFSErr] r => Path Rel File -> Sem r ProjectClosure
analyze file = do
  reqs <- readContentsParser installRequiresParser file
  pure (mkProjectClosure file reqs)

mkProjectClosure :: Path Rel File -> [Req] -> ProjectClosure
mkProjectClosure file reqs = ProjectClosure
  { closureStrategyGroup = PythonGroup
  , closureStrategyName  = "python-setuppy"
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

installRequiresParser :: Parser [Req]
installRequiresParser = prefix *> entries <* end
  where
  prefix  = skipManyTill anySingle (string "install_requires=[")
  entries = between quote quote requirementParser `sepBy` char ','
  end     = char ']'

  quote   = char '\''
