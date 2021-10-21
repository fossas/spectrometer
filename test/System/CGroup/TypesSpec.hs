{-# LANGUAGE TemplateHaskell #-}

module System.CGroup.TypesSpec (
  spec,
) where

import Path
import Path.IO (getCurrentDir, resolveFile)
import System.CGroup.Types
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

spec :: Spec
spec = do
  currentDir <- runIO getCurrentDir

  describe "resolveGroupController" $ do
    it "should work on a real world example" $ do
      cgroup <- resolveFile currentDir "test/System/CGroup/testdata/realworld/cgroup"
      mountinfo <- resolveFile currentDir "test/System/CGroup/testdata/realworld/mountinfo"

      controller <- resolveCGroupController' cgroup mountinfo "cpu"
      controller `shouldBe` Controller $(mkAbsDir "/sys/fs/cgroup/cpu")

    it "should resolve a direct mount root" $ do
      cgroup <- resolveFile currentDir "test/System/CGroup/testdata/direct/cgroup"
      mountinfo <- resolveFile currentDir "test/System/CGroup/testdata/direct/mountinfo"

      controller <- resolveCGroupController' cgroup mountinfo "cpu"
      controller `shouldBe` Controller $(mkAbsDir "/sys/fs/cgroup/cpu")

    it "should resolve subdirectories of a mount root" $ do
      cgroup <- resolveFile currentDir "test/System/CGroup/testdata/indirect/cgroup"
      mountinfo <- resolveFile currentDir "test/System/CGroup/testdata/indirect/mountinfo"

      controller <- resolveCGroupController' cgroup mountinfo "cpu"
      controller `shouldBe` Controller $(mkAbsDir "/sys/fs/cgroup/cpu/subdir")

    it "should work for cgroups v2" $ do
      cgroup <- resolveFile currentDir "test/System/CGroup/testdata/cgroupsv2/cgroup"
      mountinfo <- resolveFile currentDir "test/System/CGroup/testdata/cgroupsv2/mountinfo"

      controller <- resolveCGroupController' cgroup mountinfo "cpu"
      controller `shouldBe` Controller $(mkAbsDir "/sys/fs/cgroup/cpu")
