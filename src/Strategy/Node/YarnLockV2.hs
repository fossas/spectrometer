{-# LANGUAGE TemplateHaskell #-}

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

data Resolver = Resolver
  { resolverSupportsLocator :: Locator -> Bool
  , resolverSupportsDescriptor :: Descriptor -> Bool
  }

---------------------------

newtype YarnLockfileRaw = YarnLockfileRaw (Map Text PackageDescription)
  deriving (Show)

instance FromJSON YarnLockfileRaw where
  parseJSON = withObject "YarnLockfileRaw" $ \obj ->
    -- TODO: doc: yarn lockfile has a __manifest field that we need to kill
    YarnLockfileRaw <$> parseJSON (Object (HM.delete "__metadata" obj))

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

parseYL :: (Has Diagnostics sig m, Has ReadFS sig m) => Path b File -> m YarnLockfileRaw
parseYL = readContentsYaml

-- TODO: kill TemplateHaskell
-- TODO: kill Carrier.Diagnostics
testy :: IO String
testy = fmap (either (show . renderFailureBundle) show) . runDiagnostics . runReadFSIO . parseYL $ $(mkAbsFile "/Users/connor/Desktop/data-block-extract/yarn.lock")


-------------------

instance FromJSON Descriptor where
  parseJSON = withText "Descriptor" (tryParse descriptorP)

instance FromJSONKey Descriptor where
  fromJSONKey = FromJSONKeyTextParser (tryParse descriptorP)
  fromJSONKeyList = FromJSONKeyTextParser parsePackageKeys

splitTrim :: Text -> Text -> [Text]
splitTrim needle = map T.strip . T.splitOn needle

descriptorKeyP :: Parser [Descriptor]
descriptorKeyP = undefined

-- TODO: error messages
parsePackageKeys :: MonadFail m => Text -> m [Descriptor]
parsePackageKeys = traverse (tryParse descriptorP) . splitTrim ","

newtype YarnLockfile = YarnLockfile (Map Descriptor PackageDescription)
  deriving Show

instance FromJSON YarnLockfile where
  parseJSON = withObject "YarnLockfile" $ \obj -> do
    let obj' = HM.delete "__metadata" obj
    (packageEntries :: Map Text PackageDescription) <- parseJSON $ Object obj'
    let packageEntries' = concatMap (\(rn,package) -> maybe [] (map (,package)) $ parsePackageKeys rn) $ M.toList packageEntries
    let bar = M.fromList packageEntries'
    pure (YarnLockfile bar)

-- TODO: errors
validateYL :: YarnLockfileRaw -> YarnLockfile
validateYL (YarnLockfileRaw m) = YarnLockfile . M.fromList . concatMap (\(rn,package) -> maybe [] (map (,package)) $ parsePackageKeys rn) . M.toList $ m

testy2 :: IO ()
testy2 = (putStrLn =<<) . fmap (either (show . renderFailureBundle) (show . validateYL)) . runDiagnostics . runReadFSIO . parseYL $ $(mkAbsFile "/Users/connor/Desktop/data-block-extract/yarn.lock")

parseYL2 :: (Has Diagnostics sig m, Has ReadFS sig m) => Path b File -> m YarnLockfile
parseYL2 = readContentsYaml

testy3 :: IO ()
testy3 = (putStrLn =<<) . fmap (either (show . renderFailureBundle) show) . runDiagnostics . runReadFSIO . parseYL2 $ $(mkAbsFile "/Users/connor/Desktop/data-block-extract/yarn.lock")

newtype YarnLockfile2 = YarnLockfile2 (Map [Descriptor] PackageDescription)
  deriving Show

instance FromJSON YarnLockfile2 where
  parseJSON = withObject "YarnLockfile" (fmap YarnLockfile2 . parseJSON . Object . HM.delete "__metadata")

parseYL3 :: (Has Diagnostics sig m, Has ReadFS sig m) => Path b File -> m YarnLockfile2
parseYL3 = readContentsYaml

testy4 :: IO ()
testy4 = (putStrLn =<<) . fmap (either (show . renderFailureBundle) show) . runDiagnostics . runReadFSIO . parseYL3 $ $(mkAbsFile "/Users/connor/Desktop/data-block-extract/yarn.lock")
