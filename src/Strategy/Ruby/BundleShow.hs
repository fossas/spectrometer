module Strategy.Ruby.BundleShow
  ( discover
  , analyze

  , BundleShowDep(..)
  , buildGraph
  , bundleShowParser
  )
  where

import Prologue

import qualified Data.Map.Strict as M
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Text.Megaparsec
import           Text.Megaparsec.Char

import DepTypes
import Diagnostics
import Discovery.Walk
import Effect.Exec
import Graphing
import Types

discover :: Discover
discover = Discover
  { discoverName = "bundleshow"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Exec, Output ProjectClosure] r => Path Abs Dir -> Sem r ()
discover' = walk $ \dir _ files -> do
  case find (\f -> fileName f `elem` ["Gemfile", "Gemfile.lock"]) files of
    Nothing -> pure ()
    Just _  -> do
      res <- runError @ExecErr (analyze dir)
      traverse_ output res
      pure ()

  walkContinue

bundleShowCmd :: Command
bundleShowCmd = Command
  { cmdNames = ["bundle"]
  , cmdBaseArgs = ["show"]
  , cmdAllowErr = Never
  }

analyze :: Members '[Exec, Error ExecErr] r => Path Rel Dir -> Sem r ProjectClosure
analyze dir = do
  deps <- execParser bundleShowParser dir bundleShowCmd []
  pure (mkProjectClosure dir deps)

mkProjectClosure :: Path Rel Dir -> [BundleShowDep] -> ProjectClosure
mkProjectClosure dir deps = ProjectClosure
  { closureStrategyGroup = RubyGroup
  , closureStrategyName  = "ruby-bundleshow"
  , closureModuleDir     = dir
  , closureDependencies  = dependencies
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph deps
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

buildGraph :: [BundleShowDep] -> Graphing Dependency
buildGraph xs = unfold xs (const []) toDependency
  where
  toDependency BundleShowDep{..} =
    Dependency { dependencyType = GemType
               , dependencyName = depName
               , dependencyVersion = Just (CEq depVersion)
               , dependencyLocations = []
               , dependencyTags = M.empty
               }

data BundleShowDep = BundleShowDep
  { depName    :: Text
  , depVersion :: Text
  } deriving (Eq, Ord, Show, Generic)

type Parser = Parsec Void Text

bundleShowParser :: Parser [BundleShowDep]
bundleShowParser = concat <$> ((line <|> ignoredLine) `sepBy` eol) <* eof
  where
  isEndLine :: Char -> Bool
  isEndLine '\n' = True
  isEndLine '\r' = True
  isEndLine _    = False

  -- ignore content until the end of the line
  ignored :: Parser ()
  ignored = () <$ takeWhileP (Just "ignored") (not . isEndLine)

  ignoredLine :: Parser [BundleShowDep]
  ignoredLine = do
    ignored
    pure []

  findDep :: Parser Text
  findDep = takeWhileP (Just "dep") (/= ' ')

  findVersion :: Parser Text
  findVersion = takeWhileP (Just "version") (/= ')')

  line :: Parser [BundleShowDep]
  line = do
    _ <- chunk "  * "
    dep <- findDep
    _ <- chunk " ("
    version <- findVersion
    _ <- char ')'
    pure [BundleShowDep dep version]
