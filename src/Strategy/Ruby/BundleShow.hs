module Strategy.Ruby.BundleShow
  ( discover
  , analyze

  , BundleShowDep(..)
  , buildGraph
  , bundleShowParser
  )
  where

import Prologue

import Control.Carrier.Error.Either
import Control.Carrier.Writer.Strict
import qualified Data.Map.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char

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

discover' ::
  ( Has (Writer [ProjectClosure]) sig m
  , Has Exec sig m
  , MonadIO m
  , Effect sig
  )
  => Path Abs Dir -> m ()
discover' = walk $ \dir _ files -> do
  case find (\f -> fileName f `elem` ["Gemfile", "Gemfile.lock"]) files of
    Nothing -> pure ()
    Just _  -> do
      res <- runError @ExecErr (analyze dir)
      traverse_ (tell @[ProjectClosure] . pure) res
      pure ()

  walkContinue

bundleShowCmd :: Command
bundleShowCmd = Command
  { cmdNames = ["bundle"]
  , cmdBaseArgs = ["show"]
  , cmdAllowErr = Never
  }

analyze :: (Has Exec sig m, Has (Error ExecErr) sig m) => Path Rel Dir -> m ProjectClosure
analyze dir = mkProjectClosure dir <$> execParser bundleShowParser dir bundleShowCmd []

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
