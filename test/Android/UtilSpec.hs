module Android.UtilSpec (
  spec,
) where

import Control.Monad (forM_)
import Data.String.Conversion (toString)
import Data.Text (Text)
import Strategy.Android.Util (isDefaultAndroidDevConfig, isDefaultAndroidTestConfig)
import Test.Hspec ( describe, it, shouldBe, Spec )

defaultAndroidDevConfigs :: [Text]
defaultAndroidDevConfigs =
  [ "lintClassPath"
  , "debugRuntimeClasspath"
  ]

defaultAndroidTestConfigs :: [Text]
defaultAndroidTestConfigs =
  [ "testApiDependenciesMetadata"
  , "testReleaseApiDependenciesMetadata"
  , "testReleaseCompileOnlyDependenciesMetadata"
  , "androidTestApiDependenciesMetadata"
  , "debugAndroidTestImplementationDependenciesMetadata"
  , "releaseUnitTestImplementationDependenciesMetadata"
  ]

defaultNonDevAndTestConfigs :: [Text]
defaultNonDevAndTestConfigs =
  [ "default"
  , "androidApis"
  , "implementation"
  , "coreLibraryDesugaring"
  , "releaseRuntimeOnlyDependenciesMetadata"
  , "archives"
  ]

spec :: Spec
spec = do
  describe "isDefaultAndroidDevConfig" $ do
    forM_ defaultAndroidDevConfigs $ \candidate -> do
      it ("should return true when provided with: " <> toString candidate) $ do
        isDefaultAndroidDevConfig candidate `shouldBe` True

    forM_ defaultNonDevAndTestConfigs $ \candidate -> do
      it ("should return false when provided with: " <> toString candidate) $ do
        isDefaultAndroidDevConfig candidate `shouldBe` False

  describe "isDefaultAndroidTestConfig" $ do
    forM_ defaultAndroidTestConfigs $ \candidate -> do
      it ("should return true when provided with: " <> toString candidate) $ do
        isDefaultAndroidTestConfig candidate `shouldBe` True

    forM_ defaultNonDevAndTestConfigs $ \candidate -> do
      it ("should return false when provided with: " <> toString candidate) $ do
        isDefaultAndroidTestConfig candidate `shouldBe` False
