{-# LANGUAGE TemplateHaskell #-}

-- FIXME: kill TemplateHaskell
-- FIXME: kill Carrier.Diagnostics
module Strategy.Node.YarnLockV2 (
  ) where

import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Effect.Diagnostics
import Data.Aeson
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Effect.ReadFS
import Path
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

tryParse :: MonadFail m => Parser a -> Text -> m a
tryParse p inp = either (fail . errorBundlePretty) pure $ runParser p "" inp

data Locator = Locator
  { locatorScope :: Maybe Text
  , locatorName :: Text
  , locatorReference :: Text
  }
  deriving (Show)

instance FromJSON Locator where
  parseJSON = withText "Locator" (tryParse locatorP)

-- structUtils.tryParseLocator (strict mode):
--   string.match(/^(?:@([^/]+?)\/)?([^/]+?)(?:@(.+))$/)
locatorP :: Parser Locator
locatorP = do
  scope <- optional $ char '@' *> segment "Scope" <* char '/'
  package <- segment "Package"
  _ <- char '@'
  reference <- takeRest
  pure $ Locator scope package reference
  where
    segment :: String -> Parser Text
    segment name = takeWhile1P (Just name) (\c -> c /= '/' && c /= '@')

data Descriptor = Descriptor
  { descriptorScope :: Maybe Text
  , descriptorName :: Text
  , descriptorReference :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON Descriptor where
  parseJSON = withText "Descriptor" (tryParse descriptorP)

instance FromJSONKey Descriptor where
  fromJSONKey = FromJSONKeyTextParser (tryParse descriptorP)
  fromJSONKeyList = FromJSONKeyTextParser parsePackageKeys

-- structUtils.tryParseDescriptor (strict mode):
--   string.match(/^(?:@([^/]+?)\/)?([^/]+?)(?:@(.+))$/)
descriptorP :: Parser Descriptor
descriptorP = do
  scope <- optional $ char '@' *> segment "Scope" <* char '/'
  package <- segment "Package"
  _ <- char '@'
  reference <- takeRest
  pure $ Descriptor scope package reference
  where
    segment :: String -> Parser Text
    segment name = takeWhile1P (Just name) (\c -> c /= '/' && c /= '@')

splitTrim :: Text -> Text -> [Text]
splitTrim needle = map T.strip . T.splitOn needle

parsePackageKeys :: MonadFail m => Text -> m [Descriptor]
parsePackageKeys = traverse (tryParse descriptorP) . splitTrim ","

---------------------------

instance FromJSON PackageDescription where
  parseJSON = withObject "PackageDescription" $ \obj ->
    PackageDescription
      <$> obj .: "version"
      <*> obj .: "resolution"
      <*> (obj .:? "dependencies" .!= M.empty >>= parseDependencyDescriptors)

parseDependencyDescriptors :: MonadFail m => Map Text Text -> m [Descriptor]
parseDependencyDescriptors = traverse (\(name, reference) -> tryParse descriptorP (name <> "@" <> reference)) . M.toList

data PackageDescription = PackageDescription
  { descVersion :: Text
  , descResolution :: Locator
  , descDependencies :: [Descriptor]
  }
  deriving (Show)

parseYL :: (Has Diagnostics sig m, Has ReadFS sig m) => Path b File -> m YarnLockfile
parseYL = readContentsYaml

---------------------------

data Resolver = Resolver
  { resolverSupportsLocator :: Locator -> Bool
  , resolverSupportsDescriptor :: Descriptor -> Bool
  }

-------------------

newtype YarnLockfile = YarnLockfile (Map [Descriptor] PackageDescription)
  deriving Show

instance FromJSON YarnLockfile where
  parseJSON = withObject "YarnLockfile" (fmap YarnLockfile . parseJSON . Object . HM.delete "__metadata")

testy :: IO ()
testy = (putStrLn =<<) . fmap (either (show . renderFailureBundle) show) . runDiagnostics . runReadFSIO . parseYL $ $(mkAbsFile "/Users/connor/Desktop/data-block-extract/yarn.lock")
--testy = (putStrLn =<<) . fmap (either (show . renderFailureBundle) show) . runDiagnostics . runReadFSIO . parseYL $ $(mkAbsFile "/Users/connor/Desktop/tmp26/yarn.lock")
