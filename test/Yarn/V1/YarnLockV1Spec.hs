{-# LANGUAGE TemplateHaskell #-}

module Yarn.V1.YarnLockV1Spec (
  spec,
) where

import Control.Effect.Diagnostics
import Control.Effect.Lift
import Data.Functor (($>))
import Data.Map.Strict qualified as Map
import Data.String.Conversion (decodeUtf8)
import DepTypes (
  DepType (NodeJSType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.ReadFS (ReadFS, readContentsBS)
import GraphUtil (expectDeps', expectDirect', expectEdges')
import Path
import Strategy.Node.YarnV1.YarnLock (buildGraph)
import Test.Effect (expectationFailure', it')
import Test.Hspec (Spec, describe)
import Yarn.Lock qualified as YL

packageOne :: Dependency
packageOne =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageOne"
    , dependencyVersion = Just (CEq "1.0.0")
    , dependencyLocations = ["https://registry.npmjs.org/packageOne"]
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

packageTwo :: Dependency
packageTwo =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageTwo"
    , dependencyVersion = Just (CEq "2.0.0")
    , dependencyLocations = ["https://registry.npmjs.org/packageTwo"]
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

packageThree :: Dependency
packageThree =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageThree"
    , dependencyVersion = Just (CEq "3.0.0")
    , dependencyLocations = ["https://registry.npmjs.org/packageThree"]
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

packageFour :: Dependency
packageFour =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageFour"
    , dependencyVersion = Just (CEq "4.0.0")
    , dependencyLocations = ["https://registry.npmjs.org/packageFour"]
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

packageFive :: Dependency
packageFive =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageFive"
    , dependencyVersion = Just (CEq "5.0.0")
    , dependencyLocations = ["https://someurl.io/somefile.gz"]
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

parseFile :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Lift IO) sig m) => Path Rel File -> m YL.Lockfile
parseFile name = do
  let testdataRoot = $(mkRelDir "test/Yarn/V1/testdata/")
  testFile <- readContentsBS (testdataRoot </> name)
  case YL.parse "test/Yarn/testdata/yarn.lock" (decodeUtf8 testFile) of
    Left _ -> expectationFailure' "failed to parse" $> error "no possible value"
    Right lockfile -> pure lockfile

spec :: Spec
spec = do
  describe "buildGraph without package.json info" $ do
    it' "should produce expected structure" $ do
      yarnLock <- parseFile $(mkRelFile "simple-yarn-lock.txt")
      graph <- buildGraph yarnLock mempty
      expectDeps' [packageOne, packageTwo, packageThree, packageFour, packageFive] graph
      expectDirect' [] graph
      expectEdges'
        [ (packageOne, packageTwo)
        , (packageTwo, packageThree)
        ]
        graph

-- describe "buildGraph with promotions" $
-- it' "Should apply the correct dep environments" $ do
