{-# language TemplateHaskell #-}

module Googlesource.RepoManifestTest
  ( spec_analyze
  ) where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import DepTypes
import GraphUtil
import Parse.XML
import Strategy.Googlesource.RepoManifest
import Test.Tasty.Hspec

-- <remote name="aosp" fetch="https://android.googlesource.com" />
remoteOne :: ManifestRemote
remoteOne = ManifestRemote { remoteName     = "aosp"
                           , remoteFetch    = "https://android.googlesource.com"
                           , remoteRevision = Nothing
                           }

-- <remote fetch="https://android.othersource.com" name="othersource" revision="google/android-6.0.1_r74" />
remoteTwo :: ManifestRemote
remoteTwo = ManifestRemote { remoteName     = "othersource"
                           , remoteFetch    = "https://android.othersource.com"
                           , remoteRevision = Just "google/android-6.0.1_r74"
                           }

basicRemoteList :: [ManifestRemote]
basicRemoteList = [remoteOne, remoteTwo]

-- <default revision="refs/tags/android-10.0.0_r29"
--          remote="aosp"
--          sync-j="4" />
basicDefault :: ManifestDefault
basicDefault = ManifestDefault { defaultRemote   = Just "aosp"
                               , defaultRevision = Just "refs/tags/android-10.0.0_r29"
                               }

-- <project path="art" name="platform/art" groups="pdk" />
projectOne :: ManifestProject
projectOne = ManifestProject { projectName     = "platform/art"
                             , projectPath     = Just "art"
                             , projectRevision = ""
                             , projectRemote   = Nothing
                             }
-- <project path="bionic" name="platform/bionic" groups="pdk" revision="57b7d1574276f5e7f895c884df29f45859da74b6" />
projectTwo :: ManifestProject
projectTwo = ManifestProject { projectName     = "platform/bionic"
                             , projectPath     = Just "bionic"
                             , projectRevision = "57b7d1574276f5e7f895c884df29f45859da74b6"
                             , projectRemote   = Nothing
                             }
-- <project path="bootable/recovery" name="platform/bootable/recovery" groups="pdk" remote="othersource" />
projectThree :: ManifestProject
projectThree = ManifestProject { projectName     = "platform/bootable/recovery"
                               , projectPath     = Just "bootable/recovery"
                               , projectRevision = ""
                               , projectRemote   = Just "othersource"
                               }
-- <project path="cts" name="platform/cts" groups="cts,pdk-cw-fs,pdk-fs" />
projectFour :: ManifestProject
projectFour = ManifestProject { projectName    = "platform/cts"
                             , projectPath     = Just "cts"
                             , projectRevision = ""
                             , projectRemote   = Nothing
                             }
-- <project path="dalvik" name="platform/dalvik" groups="pdk-cw-fs,pdk-fs" />
projectFive :: ManifestProject
projectFive = ManifestProject { projectName    = "platform/dalvik"
                             , projectPath     = Just "dalvik"
                             , projectRevision = ""
                             , projectRemote   = Nothing
                             }

basicProjectList :: [ManifestProject]
basicProjectList = [projectOne, projectTwo, projectThree, projectFour, projectFive]

spec_analyze :: Spec
spec_analyze = do
  basicManifest <- runIO (TIO.readFile "test/Googlesource/testdata/manifest.xml")

  describe "repo manifest analyzer" $ do
    it "reads a file and constructs a dependency list" $ do
      case parseXML basicManifest of
        Right manifest -> do
          (manifestProjects manifest) `shouldMatchList` basicProjectList
          (manifestDefault manifest) `shouldBe` Just basicDefault
          (manifestRemotes manifest) `shouldMatchList` basicRemoteList
        Left err -> expectationFailure (T.unpack ("could not parse repo manifest file: " <> xmlErrorPretty err))
