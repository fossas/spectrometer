{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Strategy.Erlang.Rebar3Tree
  ( discover
  , analyze

  , buildGraph
  , rebar3TreeParser
  , Rebar3Dep(..)
  )
  where

import Control.Effect.Diagnostics
import Data.Foldable (find)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import DepTypes
import Discovery.Walk
import Effect.Exec
import Effect.ReadFS
import Graphing (Graphing, unfold)
import Path
import Strategy.Erlang.ConfigParser (parseConfig, ErlValue (..), ConfigValues (..), AtomText (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \dir _ files -> do
  case find (\f -> fileName f == "rebar.config") files of
    Nothing -> pure WalkContinue
    Just _  -> do
      runSimpleStrategy "erlang-rebar3tree" ErlangGroup $ analyze dir
      pure WalkSkipAll

rebar3TreeCmd :: Command
rebar3TreeCmd = Command
  { cmdName = "rebar3"
  , cmdArgs = ["tree", "-v"]
  , cmdAllowErr = Never
  }

configFile :: Path Rel File
configFile = $(mkRelFile "rebar.config")

analyze :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m ProjectClosureBody
analyze dir = do
  aliasMap <- extractAliasLookup <$> readContentsParser parseConfig (dir </> configFile)
  mkProjectClosure dir . unaliasDeps aliasMap <$> execParser rebar3TreeParser dir rebar3TreeCmd

extractAliasLookup :: ConfigValues -> M.Map Text Text
extractAliasLookup (ConfigValues erls) = foldr extract M.empty erls
  where
    extract :: ErlValue -> M.Map Text Text -> M.Map Text Text
    extract val aliasMap = aliasMap <> M.fromList (mapMaybe getAlias packages)
      where
        packages :: [ErlValue]
        packages = case val of
          ErlTuple [ErlAtom (AtomText "deps"), ErlArray deplist] -> deplist
          _ -> []
        
        getAlias :: ErlValue -> Maybe (Text, Text)
        getAlias erl = case erl of
          ErlTuple [ErlAtom (AtomText realname), ErlString _, ErlTuple [ErlAtom (AtomText "pkg"), ErlAtom (AtomText alias)]] -> Just (realname, alias)
          ErlTuple [ErlAtom (AtomText realname), ErlTuple [ErlAtom (AtomText "pkg"), ErlAtom (AtomText alias)]] -> Just (realname, alias)
          _ -> Nothing
          

unaliasDeps :: M.Map Text Text -> [Rebar3Dep] -> [Rebar3Dep]
unaliasDeps aliasMap = map unalias
  where
    unalias :: Rebar3Dep -> Rebar3Dep
    unalias dep = changeName (lookupName (depName dep) aliasMap) dep

mkProjectClosure :: Path Abs Dir -> [Rebar3Dep] -> ProjectClosureBody
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
buildGraph deps = unfold deps subDeps toDependency
  where
  toDependency Rebar3Dep{..} = 
    Dependency { dependencyType = if T.isInfixOf "github.com" depLocation then GitType else HexType
                 , dependencyName = if T.isInfixOf "github.com" depLocation then depLocation else depName
                 , dependencyVersion = Just (CEq depVersion)
                 , dependencyLocations = []
                 , dependencyEnvironments = []
                 , dependencyTags = M.empty
                 }

lookupName :: Text -> M.Map Text Text -> Text
lookupName name = M.findWithDefault name name

data Rebar3Dep = Rebar3Dep
  { depName     :: Text
  , depVersion  :: Text
  , depLocation :: Text
  , subDeps     :: [Rebar3Dep]
  } deriving (Eq, Ord, Show)

changeName :: Text -> Rebar3Dep -> Rebar3Dep
changeName name dep = dep { depName = name }

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
  ignored = () <$ takeWhileP (Just "ignored") (not . isEndLine)

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

  rebarDep :: Int -> Parser [Rebar3Dep]
  rebarDep depth = do
    _ <- chunk " "
    slashCount <- many "  │"
    _ <- satisfy (\_ -> length slashCount == depth)

    _ <- chunk "  & " <|> chunk "  ├─ " <|> chunk " ├─ " <|> chunk " └─ " 
    dep <- findName
    _ <- chunk "─"
    version <- findVersion
    _ <- chunk " ("
    location <- findLocation
    _ <- chunk ")"
   
    deps <- many $ try $ rebarRecurse $ depth + 1

    pure [Rebar3Dep dep version location (concat deps)]
  
  rebarRecurse :: Int -> Parser [Rebar3Dep]
  rebarRecurse depth = do
    _ <- chunk "\n"
    rebarDep depth
