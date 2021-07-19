module Python.Poetry.PyProjectSpec (
  spec,
) where

import Data.Map qualified as Map
import Data.Text.IO qualified as TIO

-- import Strategy.Python.Poetry.Common (buildPyProjectGraph)
import Strategy.Python.Poetry.PyProject (
  PoetryDependency (..),
  PyProject (..),
  PyProjectBuildSystem (..),
  PyProjectPoetry (..),
  PyProjectPoetryDetailedVersionDependency (..),
  PyProjectPoetryGitDependency (..),
  PyProjectPoetryPathDependency (..),
  PyProjectPoetryUrlDependency (..),
  getDependencies,
  getPoetryBuildSystem,
  parseConstraintExpr,
  pyProjectCodec,
 )
import Test.Hspec
import Toml qualified

import Data.Text (Text)
import Data.Void (Void)
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (GitType, PipType, URLType, UserType),
  Dependency (Dependency),
  VerConstraint (
    CAnd,
    CCompatible,
    CEq,
    CGreater,
    CGreaterOrEq,
    CLess,
    CLessOrEq,
    CNot,
    COr
  ),
 )
import Test.Hspec.Megaparsec hiding (err)
import Text.Megaparsec

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

shouldParseInto :: Text -> VerConstraint -> Expectation
shouldParseInto = parseMatch parseConstraintExpr

expectedPyProject :: PyProject
expectedPyProject =
  PyProject
    { pyprojectBuildSystem = Just $ PyProjectBuildSystem{buildBackend = "poetry.core.masonry.api"}
    , pyprojectPoetry =
        Just $
          PyProjectPoetry
            { name = Just "test_name"
            , version = Just "test_version"
            , description = Just "test_description"
            , dependencies =
                Map.fromList
                  [ ("flake8", PoetryTextVersion "^1.1")
                  , ("python", PoetryTextVersion "^3.9")
                  , ("flask", PyProjectPoetryGitDependencySpec $ PyProjectPoetryGitDependency{gitUrl = "https://github.com/pallets/flask.git", gitRev = Just "38eb5d3b", gitTag = Nothing, gitBranch = Nothing})
                  , ("networkx", PyProjectPoetryGitDependencySpec $ PyProjectPoetryGitDependency{gitUrl = "https://github.com/networkx/networkx.git", gitRev = Nothing, gitTag = Nothing, gitBranch = Nothing})
                  , ("numpy", PyProjectPoetryGitDependencySpec $ PyProjectPoetryGitDependency{gitUrl = "https://github.com/numpy/numpy.git", gitRev = Nothing, gitTag = Just "v0.13.2", gitBranch = Nothing})
                  , ("requests", PyProjectPoetryGitDependencySpec $ PyProjectPoetryGitDependency{gitUrl = "https://github.com/kennethreitz/requests.git", gitRev = Nothing, gitTag = Nothing, gitBranch = Just "next"})
                  , ("my-packageUrl", PyProjectPoetryUrlDependencySpec $ PyProjectPoetryUrlDependency{sourceUrl = "https://example.com/my-package-0.1.0.tar.gz"})
                  , ("my-packageFile", PyProjectPoetryPathDependencySpec $ PyProjectPoetryPathDependency{sourcePath = "../my-package/dist/my-package-0.1.0.tar.gz"})
                  , ("my-packageDir", PyProjectPoetryPathDependencySpec $ PyProjectPoetryPathDependency{sourcePath = "../my-package/"})
                  , ("black", PyProjectPoetryDetailedVersionDependencySpec $ PyProjectPoetryDetailedVersionDependency{poetryDependencyVersion = "19.10b0"})
                  ]
            , devDependencies =
                Map.fromList
                  [("pytest", PoetryTextVersion "*")]
            }
    }

expectedDeps :: [Dependency]
expectedDeps =
  [ dep PipType "flake8" (Just $ CCompatible "1.1") prodEnvs
  , dep GitType "https://github.com/pallets/flask.git" (Just $ CEq "38eb5d3b") prodEnvs
  , dep GitType "https://github.com/networkx/networkx.git" Nothing prodEnvs
  , dep GitType "https://github.com/numpy/numpy.git" (Just $ CEq "v0.13.2") prodEnvs
  , dep GitType "https://github.com/kennethreitz/requests.git" (Just $ CEq "next") prodEnvs
  , dep URLType "https://example.com/my-package-0.1.0.tar.gz" Nothing prodEnvs
  , dep UserType "../my-package/dist/my-package-0.1.0.tar.gz" Nothing prodEnvs
  , dep UserType "../my-package/" Nothing prodEnvs
  , dep PipType "black" (Just $ CEq "19.10b0") $ prodEnvs
  , dep PipType "pytest" Nothing devEnvs
  ]
  where
    dep :: DepType -> Text -> Maybe VerConstraint -> [DepEnvironment] -> Dependency
    dep t n v e = Dependency t n v [] e Map.empty
    prodEnvs :: [DepEnvironment]
    prodEnvs = [EnvProduction]
    devEnvs :: [DepEnvironment]
    devEnvs = [EnvDevelopment]

spec :: Spec
spec = do
  nominalContents <- runIO (TIO.readFile "test/Python/Poetry/testdata/pyproject1.toml")
  emptyContents <- runIO (TIO.readFile "test/Python/Poetry/testdata/pyproject2.toml")

  describe "pyProjectCodec" $ do
    describe "when provided with all possible types of dependency sources" $ do
      it "should parse pyrproject file with all source types" $
        Toml.decode pyProjectCodec nominalContents `shouldBe` Right expectedPyProject

  describe "getDependencies" $ do
    it "should get all dependencies" $
      getDependencies expectedPyProject `shouldMatchList` expectedDeps

  describe "getPoetryBuildSystem" $ do
    describe "when provided with poetry build system" $ do
      it "should return true" $
        getPoetryBuildSystem <$> (Toml.decode pyProjectCodec nominalContents)
          `shouldBe` Right (Just "poetry.core.masonry.api")

    describe "when not provided with any build system" $ do
      it "should return nothing" $
        getPoetryBuildSystem <$> Toml.decode pyProjectCodec emptyContents
          `shouldBe` Right Nothing

  describe "parseConstraintExpr" $ do
    it "should parse equality constraint" $ do
      "1.1" `shouldParseInto` (CEq "1.1")
      "==1.1" `shouldParseInto` (CEq "1.1")
      "=1.1" `shouldParseInto` (CEq "1.1")

    it "should parse greater than or equal constraint" $
      ">=2.1" `shouldParseInto` (CGreaterOrEq "2.1")

    it "should parse less than or equal constraint" $
      "<=2.1" `shouldParseInto` (CLessOrEq "2.1")

    it "should parse greater than constraint" $
      ">2.1" `shouldParseInto` (CGreater "2.1")

    it "should parse less than constraint" $
      "<2.1" `shouldParseInto` (CLess "2.1")

    it "should parse not equal to constraint" $
      "!=3.1" `shouldParseInto` (CNot "3.1")

    it "should parse wildcard constraint" $
      "*" `shouldParseInto` (CEq "*")

    it "should parse caret (^) constraint" $
      "^4.2.1" `shouldParseInto` (CCompatible "4.2.1")

    it "should parse tidal (~) constraint" $ do
      "~5.1" `shouldParseInto` (CCompatible "5.1")
      "~=5.1" `shouldParseInto` (CCompatible "5.1")

    describe "when provided with multiple constraints" $ do
      it "should parse OR operator" $
        "6.1 || 6.2" `shouldParseInto` (COr (CEq "6.1") (CEq "6.2"))

      it "should parse AND operator" $
        ">=7.1, <7.7" `shouldParseInto` (CAnd (CGreaterOrEq "7.1") (CLess "7.7"))

      it "should give precedence to the AND operator" $
        ">=8, 9.1.1 || 9.1.2, <=8.9.9 " `shouldParseInto` CAnd (CAnd (CGreaterOrEq "8") (COr (CEq "9.1.1") (CEq "9.1.2"))) (CLessOrEq "8.9.9")

    describe "when provided with irregular spacing or tabs" $ do
      it "should parse expressions" $ do
        "  1.1" `shouldParseInto` (CEq "1.1")
        "==1.1  " `shouldParseInto` (CEq "1.1")
        "= 1.1" `shouldParseInto` (CEq "1.1")
        "6.1||6.2" `shouldParseInto` (COr (CEq "6.1") (CEq "6.2"))
        " 6.1 ||6.2   " `shouldParseInto` (COr (CEq "6.1") (CEq "6.2"))
        ">=7.1,<7.7" `shouldParseInto` (CAnd (CGreaterOrEq "7.1") (CLess "7.7"))
        ">=7.1,< 7.7" `shouldParseInto` (CAnd (CGreaterOrEq "7.1") (CLess "7.7"))
        " >=7.1,< 7.7" `shouldParseInto` (CAnd (CGreaterOrEq "7.1") (CLess "7.7"))
        "<7.7, >=7.1" `shouldParseInto` (CAnd (CLess "7.7") (CGreaterOrEq "7.1"))
        "\t1.1" `shouldParseInto` (CEq "1.1")
        "<7.7,\t>=7.1" `shouldParseInto` (CAnd (CLess "7.7") (CGreaterOrEq "7.1"))
