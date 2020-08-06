module App.Fossa.FossaAPIV1Spec (spec) where

import App.Fossa.FossaAPIV1 (Issue (..), IssueRule (..), IssueType (..), Issues (..))
import Data.Aeson (FromJSON, ToJSON, fromJSON, toJSON)
import Data.Text (Text)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec
import Test.Hspec.Hedgehog
import Prelude

spec :: Spec
spec = do
  describe "Issues ToJSON/FromJSON instances" $ do
    it "are roundtrippable" $ hedgehog $ do
      issues <- forAll genIssues
      roundtripJson issues

genIssues :: Gen Issues
genIssues =
  Issues
    <$> Gen.int (Range.linear minBound maxBound)
    <*> Gen.list (Range.linear 0 100) genIssue
    <*> arbitraryText

genIssue :: Gen Issue
genIssue =
  Issue
    <$> Gen.int (Range.linear minBound maxBound)
    <*> Gen.maybe arbitraryText
    <*> Gen.bool
    <*> arbitraryText
    <*> genIssueType
    <*> Gen.maybe genIssueRule

genIssueType :: Gen IssueType
genIssueType =
  Gen.element
    [ IssuePolicyConflict,
      IssuePolicyFlag,
      IssueVulnerability,
      IssueUnlicensedDependency,
      IssueOutdatedDependency,
      IssueOther "something else"
    ]

genIssueRule :: Gen IssueRule
genIssueRule = IssueRule <$> Gen.maybe arbitraryText

arbitraryText :: Gen Text
arbitraryText = Gen.text (Range.linear 0 100) Gen.unicodeAll

roundtripJson :: (MonadTest m, Show a, Eq a, ToJSON a, FromJSON a) => a -> m ()
roundtripJson a = tripping a toJSON fromJSON
