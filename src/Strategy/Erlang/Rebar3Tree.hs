module Strategy.Erlang.Rebar3Tree
  ( discover
  , analyze

  , buildGraph
  , rebar3TreeParser
  , Rebar3Dep(..)
  )
  where

import Prologue

import Control.Carrier.Error.Either
import qualified Data.Map.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char

import DepTypes
import Discovery.Walk
import Effect.Exec
import Graphing (Graphing)
import qualified Graphing
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \dir _ files -> do
  case find (\f -> fileName f `elem` ["rebar.config"]) files of
    Nothing -> pure ()
    Just _  -> runSimpleStrategy "erlang-rebar3tree" ErlangGroup $ analyze dir

  pure WalkContinue

rebar3TreeCmd :: Command
rebar3TreeCmd = Command
  { cmdNames = ["rebar3"]
  , cmdBaseArgs = ["tree", "-v"]
  , cmdAllowErr = Never
  }

analyze :: (Has Exec sig m, Has (Error ExecErr) sig m) => Path Rel Dir -> m ProjectClosureBody
analyze dir = mkProjectClosure dir <$> execParser rebar3TreeParser dir rebar3TreeCmd []

mkProjectClosure :: Path Rel Dir -> [Rebar3Dep] -> ProjectClosureBody
mkProjectClosure dir deps = ProjectClosureBody
  { bodyModuleDir    = dir
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph deps
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

buildGraph :: [Rebar3Dep] -> Graphing Dependency
buildGraph = Graphing.fromList . map toDependency
  where
  toDependency Rebar3Dep{..} =
    Dependency { dependencyType = HexType
               , dependencyName = depName
               , dependencyVersion = Just (CEq depVersion)
               , dependencyLocations = []
               , dependencyEnvironments = []
               , dependencyTags = M.empty
               }

data Rebar3Dep = Rebar3Dep
  { depName     :: Text
  , depVersion  :: Text
  , depLocation :: Text
  , subDeps     :: [Rebar3Dep]
  } deriving (Eq, Ord, Show, Generic)

type Parser = Parsec Void Text

rebar3TreeParser :: Parser [Rebar3Dep]
rebar3TreeParser = concat <$> ((try (rebarDep 0) <|> ignoredLine) `sepBy` eol) <* eof
  where
  isEndLine :: Char -> Bool
  isEndLine '\n' = True
  isEndLine '\r' = True
  isEndLine _    = False

  -- ignore content until the end of the line
  ignored :: Parser ()
  ignored = do
        i <- takeWhileP (Just "ignored") (not . isEndLine)
        _ <- traceM $ show i
        pure ()

  ignoredLine :: Parser [Rebar3Dep]
  ignoredLine = do
    ignored
    pure []

  findName :: Parser Text
  findName = takeWhileP (Just "dep") (/= '─')

  findVersion :: Parser Text
  findVersion = takeWhileP (Just "version") (/= ' ')

  findLocation :: Parser Text
  findLocation = takeWhileP (Just "location") (/= ')')

  rebarDep :: Integer -> Parser [Rebar3Dep]
  rebarDep depth = do
    _ <- chunk " "
    countSlash <- many "  │"
    _ <- satisfy (\_ -> (fromIntegral (length countSlash)) == depth)

    _ <- chunk "  & " <|> chunk "  ├─ " <|> chunk " ├─ " <|> chunk " └─ " 
    dep <- findName
    _ <- chunk "─"
    version <- findVersion
    _ <- chunk " ("
    location <- findLocation
    _ <- chunk ")"
   
    deps <- many $ try $ rebarRecurse $ depth + 1

    pure [Rebar3Dep dep version location (concat deps)]
  
  rebarRecurse :: Integer -> Parser [Rebar3Dep]
  rebarRecurse depth = do
    _ <- chunk "\n"
    deps <- rebarDep depth
    pure deps