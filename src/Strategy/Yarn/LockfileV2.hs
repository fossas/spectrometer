module Strategy.Yarn.LockfileV2 (
  YarnLockfile (..),
  Locator (..),
  Descriptor (..),
) where

import Data.Aeson
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

---------- Types

-- FIXME: doc link
newtype YarnLockfile = YarnLockfile (Map [Descriptor] PackageDescription)
  deriving (Eq, Ord, Show)

-- FIXME: doc link
data Locator = Locator
  { locatorScope :: Maybe Text
  , locatorName :: Text
  , locatorReference :: Text
  }
  deriving (Eq, Ord, Show)

-- FIXME: doc link
data Descriptor = Descriptor
  { descriptorScope :: Maybe Text
  , descriptorName :: Text
  , descriptorRange :: Text
  }
  deriving (Eq, Ord, Show)

-- FIXME: doc link
data PackageDescription = PackageDescription
  { descVersion :: Text
  , descResolution :: Locator
  , descDependencies :: [Descriptor]
  }
  deriving (Eq, Ord, Show)

---------- Decoding from JSON

-- | The yarn lockfile is a @Map [Descriptor] PackageDescription@ with one
-- additional top-level field: @__metadata@
--
-- To decode the lockfile, we kill the metadata field, and run parseJSON again
-- on the object
-- FIXME: doc link
instance FromJSON YarnLockfile where
  parseJSON = withObject "YarnLockfile" (fmap YarnLockfile . parseJSON . Object . HM.delete "__metadata")

-- FIXME: doc link
instance FromJSON Locator where
  parseJSON = withText "Locator" (tryParse locatorP)

-- FIXME: doc link
instance FromJSON Descriptor where
  parseJSON = withText "Descriptor" (tryParse descriptorP)

-- FIXME: doc link
instance FromJSONKey Descriptor where
  fromJSONKey = FromJSONKeyTextParser (tryParse descriptorP)
  fromJSONKeyList = FromJSONKeyTextParser parsePackageKeys

-- FIXME: doc link
parsePackageKeys :: MonadFail m => Text -> m [Descriptor]
parsePackageKeys = traverse (tryParse descriptorP) . splitTrim ","

instance FromJSON PackageDescription where
  parseJSON = withObject "PackageDescription" $ \obj ->
    PackageDescription
      <$> obj .: "version"
      <*> obj .: "resolution"
      <*> (obj .:? "dependencies" .!= M.empty >>= parseDependencyDescriptors)

-- | Rather than storing dependencies as a flat list of Descriptors, the yarn
-- lockfile stores them as key/value pairs, split on the last "@" in a
-- descriptor
--
-- We re-construct the raw descriptor by rejoining on "@" before running the parser
-- FIXME: doc link
parseDependencyDescriptors :: MonadFail m => Map Text Text -> m [Descriptor]
parseDependencyDescriptors = traverse (\(name, range) -> tryParse descriptorP (name <> "@" <> range)) . M.toList

---------- Text field Parsers

type Parser = Parsec Void Text

tryParse :: MonadFail m => Parser a -> Text -> m a
tryParse p = either (fail . errorBundlePretty) pure . runParser p ""

-- | Locator and Descriptor fields are both parsed identically.
--
-- From yarn's structUtils.tryParseDescriptor/tryParselocator (in strict mode):
--
-- @
--     string.match(/^(?:@([^/]+?)\/)?([^/]+?)(?:@(.+))$/)
-- @
--
-- ..with the three matched fields referring to:
-- - package scope
-- - package name
-- - package range (descriptor) or package reference (locator)
packageRefP :: Parser (Maybe Text, Text, Text)
packageRefP = do
  scope <- optional $ char '@' *> segment "Scope" <* char '/'
  package <- segment "Package"
  _ <- char '@'
  rest <- takeRest
  pure (scope, package, rest)
  where
    segment :: String -> Parser Text
    segment name = takeWhile1P (Just name) (\c -> c /= '/' && c /= '@')

descriptorP :: Parser Descriptor
descriptorP = (\(scope, package, range) -> Descriptor scope package range) <$> packageRefP

locatorP :: Parser Locator
locatorP = (\(scope, package, reference) -> Locator scope package reference) <$> packageRefP

---------- Misc

-- | 'Data.Text.splitOn', but trims surrounding whitespace from the results
splitTrim :: Text -> Text -> [Text]
splitTrim needle = map T.strip . T.splitOn needle
