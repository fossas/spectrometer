{-# LANGUAGE TemplateHaskell #-}
module Strategy.Carthage
  ( discover
  , analyze
  , strategy
  ) where

import qualified Prelude as Unsafe
import Prologue

import Data.Char (isSpace)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Text.Megaparsec hiding (try)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Diagnostics
import Discovery.Walk
import DepTypes
import Effect.Error
import Effect.Grapher
import Effect.ReadFS
import qualified Graphing as G
import Types

discover :: Discover
discover = Discover
  { discoverName = "carthage"
  , discoverFunc = discover'
  }

discover' :: Members '[Embed IO, Output ConfiguredStrategy] r => Path Abs Dir -> Sem r ()
discover' = walk $ \_ subdirs files ->
  case find (\f -> fileName f == "Cartfile.resolved") files of
    Nothing -> walkContinue
    Just file -> do
      output (configure file)
      walkSkipAll subdirs

strategy :: Strategy BasicFileOpts
strategy = Strategy
  { strategyName = "carthage"
  , strategyAnalyze = analyze
  , strategyModule = parent . targetFile
  , strategyOptimal = Optimal
  , strategyComplete = Complete
  }

analyze :: Members '[ReadFS, Error ReadFSErr] r => BasicFileOpts -> Sem r (G.Graphing Dependency)
analyze (BasicFileOpts path) = fmap (G.gmap toDependency) . evalGrapher $ do
  -- We only care about top-level resolved cartfile errors
  cartfile <- readContentsParser resolvedCartfileParser path

  for_ (resolvedEntries cartfile) $ \entry -> do
    direct entry
    descend (parent path </> $(mkRelDir "Carthage/Checkouts")) entry

  where

  -- FIXME: kill ReadFSErr/try
  descend :: Members '[ReadFS, Error ReadFSErr, Grapher ResolvedEntry] r
          => Path Rel Dir -- Checkouts directory
          -> ResolvedEntry
          -> Sem r ()
  descend checkoutsDir entry = do
    let checkoutName = T.unpack $ entryToCheckoutName entry

    case parseRelDir checkoutName of
      Nothing -> pure () -- TODO?
      Just path -> do
        let checkoutPath :: Path Rel Dir
            checkoutPath = checkoutsDir </> path

        deeper <- parseDeep (checkoutPath </> $(mkRelFile "Cartfile.resolved"))
        traverse_ (edge entry) deeper

  parseDeep :: Members '[ReadFS, Error ReadFSErr, Grapher ResolvedEntry] r
            => Path Rel File
            -> Sem r [ResolvedEntry]
  parseDeep path = do
    -- FIXME: kill 'try'
    (maybeCartfile :: Either ReadFSErr ResolvedCartfile) <- try (readContentsParser resolvedCartfileParser path)
    case maybeCartfile of
      Left _ -> pure [] -- TODO?
      Right cartfile -> do
        for_ (resolvedEntries cartfile) $ \entry -> do
          descend (parent path </> $(mkRelDir "Carthage/Checkouts")) entry
        pure (resolvedEntries cartfile)

entryToCheckoutName :: ResolvedEntry -> Text
entryToCheckoutName entry =
  case resolvedType entry of
    GitType -> resolvedName entry
    -- this is safe because T.splitOn always returns a non-empty list
    GithubType -> Unsafe.last . T.splitOn "/" $ resolvedName entry
    BinaryType -> resolvedName entry

entryToDepName :: ResolvedEntry -> Text
entryToDepName entry =
  case resolvedType entry of
    GitType -> resolvedName entry
    GithubType -> "https://github.com/" <> resolvedName entry
    BinaryType -> resolvedName entry

toDependency :: ResolvedEntry -> Dependency
toDependency entry = Dependency
  { dependencyType = CarthageType
  , dependencyName = resolvedName entry -- TODO: fix for github
  , dependencyVersion = Just (CEq (resolvedVersion entry))
  , dependencyTags = M.empty
  , dependencyLocations = [] -- TODO: git location?
  }

-- Exemplary Cartfile.resolved:
-- https://github.com/Carthage/Carthage/blob/8b34a90f607c5da85c75fee1f0cf20ae742bdeb2/Tests/CarthageKitTests/Resources/TestResolvedCartfile.resolved
--
-- Additional documentation for cartfile can be found here:
-- https://github.com/Carthage/Carthage/blob/master/Documentation/Artifacts.md
resolvedCartfileParser :: Parser ResolvedCartfile
resolvedCartfileParser = ResolvedCartfile <$> many parseSingleEntry <* eof

parseSingleEntry :: Parser ResolvedEntry
parseSingleEntry = L.nonIndented scn $ do
  entryType <- lexeme $ choice
    [ GithubType <$ chunk "github"
    , GitType    <$ chunk "git"
    , BinaryType <$ chunk "binary" -- TODO: validate the assumption that it's structured the same way
    ]

  entryName    <- word
  entryVersion <- word
  _ <- scn

  pure (ResolvedEntry entryType entryName entryVersion)

-- TODO: common lexer stuff
word :: Parser Text
word = T.pack <$> choice
  [ lexeme (char '\"' *> someTill (satisfy (not . isSpace)) (char '\"'))
  -- TODO: is there a combinator for (not . isSpace) already?
  , lexeme (some (satisfy (not . isSpace)))
  ]

scn :: Parser ()
scn =  L.space space1 empty empty

sc :: Parser ()
sc =  L.space (void $ some (char ' ')) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

type Parser = Parsec Void Text

newtype ResolvedCartfile = ResolvedCartfile
  { resolvedEntries :: [ResolvedEntry]
  } deriving (Eq, Ord, Show, Generic)

data ResolvedEntry = ResolvedEntry
  { resolvedType    :: EntryType
  , resolvedName    :: Text
  , resolvedVersion :: Text
  } deriving (Eq, Ord, Show, Generic)

data EntryType = GithubType | GitType | BinaryType
  deriving (Eq, Ord, Show, Generic)

configure :: Path Rel File -> ConfiguredStrategy
configure = ConfiguredStrategy strategy . BasicFileOpts
