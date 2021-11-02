module Perl.PerlSpec (
  spec,
) where

import Data.Aeson (decodeFileStrict')
import Data.Map.Strict (empty, fromList)
import Data.Set (singleton)
import Data.Text (Text)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import DepTypes
import GraphUtil (expectDeps)
import Strategy.Perl (PackageName (PackageName), PerlMeta (..), buildGraph)
import Test.Hspec (
  Spec,
  describe,
  expectationFailure,
  it,
  shouldBe,
 )

perl :: (PackageName, Maybe Text)
perl = (PackageName "perl", Just "5.006")

expectedContentFromJson :: PerlMeta
expectedContentFromJson =
  PerlMeta
    { version = 2.0
    , runtimeRequires = Just $ fromList [(PackageName "Carp", Just "0"), perl]
    , buildRequires = Just $ fromList [perl]
    , testRequires = Just $ fromList [perl]
    , developRequires = Just $ fromList [(PackageName "Dist::Zilla", Just "5"), perl]
    , configureRequires = Just $ fromList [(PackageName "ExtUtils::MakeMaker", Just "0"), perl]
    }

expectedContentFromYaml :: PerlMeta
expectedContentFromYaml =
  PerlMeta
    { version = 1.4
    , runtimeRequires = Just $ fromList [(PackageName "Archive::Zip", Just "0"), perl]
    , buildRequires = Just $ fromList [(PackageName "Compress::Zlib", Just "0")]
    , testRequires = Nothing
    , developRequires = Nothing
    , configureRequires = Just $ fromList [(PackageName "ExtUtils::MakeMaker", Just "0")]
    }

mkDependency :: Text -> Text -> DepEnvironment -> Dependency
mkDependency name version env = Dependency CpanType name (Just $ CEq version) [] (singleton env) empty

spec :: Spec
spec = do
  describe "parse" $ do
    it "should parse json file correctly" $ do
      resolvedFile <- decodeFileStrict' "test/Perl/testdata/Meta.json"
      resolvedFile `shouldBe` Just expectedContentFromJson

    it "should parse yaml file correctly" $ do
      resolvedFile <- decodeFileEither "test/Perl/testdata/Meta.yml"
      case resolvedFile of
        Left err -> expectationFailure ("failed to parse yaml file" <> show (prettyPrintParseException err))
        Right val -> val `shouldBe` Just expectedContentFromYaml

  describe "buildGraph" $
    it "should build graph" $ do
      let graph = buildGraph expectedContentFromJson
      let expectedDeps =
            [ mkDependency "Carp" "0" EnvProduction
            , mkDependency "Dist::Zilla" "5" EnvDevelopment
            , mkDependency "ExtUtils::MakeMaker" "0" EnvDevelopment
            ]
      expectDeps expectedDeps graph
