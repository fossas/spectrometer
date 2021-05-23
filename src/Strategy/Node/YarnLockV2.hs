module Strategy.Node.YarnLockV2 (
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

data Locator = Locator
  { locatorScope :: Maybe Text
  , locatorName :: Text
  , locatorReference :: Text
  }

-- structUtils.tryParseLocator (strict mode):
--   string.match(/^(?:@([^/]+?)\/)?([^/]+?)(?:@(.+))$/)
locatorP :: Parser Locator
locatorP = do
  scope <- optional $ char '@' *> segment "Scope" <* char '/'
  package <- segment "Package"
  reference <- takeRest
  pure $ Locator scope package reference
    where
      segment :: String -> Parser Text
      segment name = takeWhile1P (Just name) (/= '/')


data Descriptor = Descriptor
  { descriptorScope :: Maybe Text
  , descriptorName :: Text
  , descriptorReference :: Text
  }

-- structUtils.tryParseDescriptor (strict mode):
--   string.match(/^(?:@([^/]+?)\/)?([^/]+?)(?:@(.+))$/)
descriptorP :: Parser Descriptor
descriptorP = do
  scope <- optional $ char '@' *> segment "Scope" <* char '/'
  package <- segment "Package"
  reference <- takeRest
  pure $ Descriptor scope package reference
    where
      segment :: String -> Parser Text
      segment name = takeWhile1P (Just name) (/= '/')

data Resolver = Resolver
  { resolverSupportsLocator :: Locator -> Bool
  , resolverSupportsDescriptor :: Descriptor -> Bool
  }
