module App.Fossa.Report.AttributionSpec
  ( spec,
  )
where

import App.Fossa.Report.Attribution
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Prologue
import Test.Hspec
import Test.Hspec.Hedgehog

defaultRange :: Range Int
defaultRange = Range.linear 0 50

genLicense :: Gen License
genLicense =
  License
    <$> (LicenseName <$> arbitraryText)
    <*> (LicenseContents <$> arbitraryText)

genProject :: Gen Project
genProject = Project <$> arbitraryText <*> arbitraryText

genDependency :: Gen Dependency
genDependency =
  Dependency
    <$> arbitraryText
    <*> arbitraryText
    <*> arbitraryText
    <*> Gen.bool
    <*> arbitraryText
    <*> Gen.list defaultRange arbitraryText
    <*> Gen.maybe arbitraryText
    <*> Gen.list defaultRange genLicense
    <*> Gen.list defaultRange genLicense
    <*> arbitraryText
    <*> Gen.list defaultRange arbitraryText
    <*> Gen.list defaultRange arbitraryText
    <*> arbitraryText
    <*> arbitraryText

genAttribution :: Gen Attribution
genAttribution =
  Attribution
    <$> genProject
    <*> Gen.list defaultRange genDependency
    <*> Gen.list defaultRange genDependency
    <*> genLicenseMap

tuplify :: Monad m => m a -> m b -> m (a, b)
tuplify a b = do
  a' <- a
  b' <- b
  return (a', b')

genLicenseMap :: Gen (Map LicenseName LicenseContents)
genLicenseMap = do
  let genName = LicenseName <$> arbitraryText
  let genContents = LicenseContents <$> arbitraryText
  Gen.map defaultRange (tuplify genName genContents)

arbitraryText :: Gen Text
arbitraryText = Gen.text (Range.linear 3 25) Gen.unicodeAll

spec :: Spec
spec =
  describe "Attribution ToJSON/FromJSON instances"
    $ modifyMaxSuccess (const 20)
    $ it "are roundtrippable"
    $ hedgehog
    $ do
      attr <- forAll genAttribution
      roundtripJson attr

roundtripJson :: (MonadTest m, Show a, Eq a, ToJSON a, FromJSON a) => a -> m ()
roundtripJson a = tripping a toJSON fromJSON
