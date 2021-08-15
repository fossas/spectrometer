module App.Fossa.VSI.Types (
  Locator (..),
  LocatorParseError (..),
  parseLocator,
  isUserDefined,
  userDefinedFetcher,
  toDependency,
) where

import Control.Effect.Diagnostics (ToDiagnostic, renderDiagnostic)
import Data.Text (Text)
import DepTypes (DepType (..), Dependency (..), VerConstraint (CEq))
import Effect.Logger (Pretty (pretty), viaShow)
import Srclib.Converter (fetcherToDepType)
import Srclib.Types qualified as Srclib

-- | VSI supports a subset of possible Locators.
-- Specifically, all VSI locators must have a valid revision.
data Locator = Locator
  { locatorFetcher :: Text
  , locatorProject :: Text
  , locatorRevision :: Text
  }
  deriving (Eq, Ord, Show)

parseLocator :: Text -> Either LocatorParseError Locator
parseLocator a = do
  let plain = Srclib.parseLocator a
  validateLocator plain

newtype LocatorParseError = RevisionRequired Srclib.Locator
  deriving (Eq, Ord, Show)

instance ToDiagnostic LocatorParseError where
  renderDiagnostic (RevisionRequired locator) =
    "Revision is required on locator: " <> viaShow locator

toDependency :: Locator -> Either ToDependencyError Dependency
toDependency locator =
  Dependency
    <$> validateDepType locator
    <*> Right (locatorProject locator)
    <*> (Right . Just . CEq $ locatorRevision locator)
    <*> Right []
    <*> Right []
    <*> Right mempty

validateDepType :: Locator -> Either ToDependencyError DepType
validateDepType locator = case fetcherToDepType (locatorFetcher locator) of
  Nothing -> Left $ UnsupportedLocator locator
  Just dt -> Right dt

newtype ToDependencyError = UnsupportedLocator Locator
  deriving (Eq, Ord, Show)

instance ToDiagnostic ToDependencyError where
  renderDiagnostic (UnsupportedLocator locator) =
    "Unsupported locator: Cannot convert fetcher "
      <> pretty (locatorFetcher locator)
      <> " to known dependency type. Locator: "
      <> viaShow locator

validateLocator :: Srclib.Locator -> Either LocatorParseError Locator
validateLocator loc =
  Locator
    <$> Right (Srclib.locatorFetcher loc)
    <*> Right (Srclib.locatorProject loc)
    <*> validateRevision loc

validateRevision :: Srclib.Locator -> Either LocatorParseError Text
validateRevision loc = case (Srclib.locatorRevision loc) of
  Nothing -> Left $ RevisionRequired loc
  Just r -> Right r

isUserDefined :: Locator -> Bool
isUserDefined loc = locatorFetcher loc == userDefinedFetcher

userDefinedFetcher :: Text
userDefinedFetcher = "iat"
