module Crystal.ShardYmlSpec (
  spec,
) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Yaml (decodeEither')
import DepTypes
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Strategy.Crystal.ShardYml (
  GitSource (..),
  PackageName (..),
  ShardYamlDepGitSource (..),
  ShardYmlContent (..),
  ShardYmlDepSource (..),
  buildGraph,
 )
import Test.Hspec

gitOrigin :: GitSource
gitOrigin = Git "https://some.git"

githubOrigin :: GitSource
githubOrigin = Github "github_username/repo"

gitlabOrigin :: GitSource
gitlabOrigin = Gitlab "gitlab_username/repo"

bitBucketOrigin :: GitSource
bitBucketOrigin = BitBucket "bitbucket_username/repo"

gitSourceOnly :: GitSource -> ShardYmlDepSource
gitSourceOnly git = ShardYamlGitSource $ ShardYamlDepGitSource git Nothing Nothing Nothing Nothing

gitSourceWithBranch :: GitSource -> Text -> ShardYmlDepSource
gitSourceWithBranch git branch = ShardYamlGitSource $ ShardYamlDepGitSource git (Just branch) Nothing Nothing Nothing

gitSourceWithCommit :: GitSource -> Text -> ShardYmlDepSource
gitSourceWithCommit git commit = ShardYamlGitSource $ ShardYamlDepGitSource git Nothing Nothing (Just commit) Nothing

gitSourceWithTag :: GitSource -> Text -> ShardYmlDepSource
gitSourceWithTag git tag = ShardYamlGitSource $ ShardYamlDepGitSource git Nothing (Just tag) Nothing Nothing

gitSourceWithVersion :: GitSource -> Text -> ShardYmlDepSource
gitSourceWithVersion git version = ShardYamlGitSource $ ShardYamlDepGitSource git Nothing Nothing Nothing (Just version)

pathSource :: Text -> ShardYmlDepSource
pathSource = ShardYamlDepPathSource

newGitOriginDep :: Maybe VerConstraint -> DepEnvironment -> Dependency
newGitOriginDep version env =
  Dependency
    GitType
    "https://some.git"
    version
    []
    [env]
    Map.empty

spec :: Spec
spec = do
  specFile <- runIO (BS.readFile "test/Crystal/testdata/shard.yml")
  describe "parse shard.yml" $
    it "should parse dependencies" $ do
      let expectedPubSpecContent =
            ShardYmlContent
              { dependencies =
                  Just $
                    Map.fromList
                      [ (PackageName "a", gitSourceWithBranch gitOrigin "develop")
                      , (PackageName "b", gitSourceWithCommit gitOrigin "005dc")
                      , (PackageName "c", gitSourceWithTag gitOrigin "someTag")
                      , (PackageName "d", gitSourceWithVersion gitOrigin "<= 2.3.1")
                      , (PackageName "e", gitSourceOnly githubOrigin)
                      , (PackageName "f", gitSourceOnly gitlabOrigin)
                      , (PackageName "g", gitSourceOnly bitBucketOrigin)
                      , (PackageName "h", gitSourceOnly gitOrigin)
                      , (PackageName "i", gitSourceWithBranch githubOrigin "develop")
                      , (PackageName "j", pathSource "./../local-path")
                      ]
              , devDependencies =
                  Just $
                    Map.fromList
                      [ (PackageName "k", pathSource "./../local-path-dev")
                      , (PackageName "l", gitSourceOnly gitOrigin)
                      ]
              }
      case decodeEither' specFile of
        Right res -> res `shouldBe` expectedPubSpecContent
        Left err -> expectationFailure $ "failed to parse: " <> show err

  describe "buildGraph" $ do
    it "should resolve url for github source" $ do
      let graph =
            buildGraph $
              ShardYmlContent
                (Just $ Map.fromList [(PackageName "a", gitSourceWithBranch githubOrigin "develop")])
                Nothing

      let dep =
            Dependency
              GitType
              "https://github.com/github_username/repo.git"
              (Just $ CEq "develop")
              []
              [EnvProduction]
              Map.empty

      expectDirect [dep] graph
      expectDeps [dep] graph
      expectEdges [] graph

    it "should resolve url for gitlab source" $ do
      let graph =
            buildGraph $
              ShardYmlContent
                (Just $ Map.fromList [(PackageName "a", gitSourceWithBranch gitlabOrigin "release")])
                Nothing

      let dep =
            Dependency
              GitType
              "https://gitlab.com/gitlab_username/repo.git"
              (Just $ CEq "release")
              []
              [EnvProduction]
              Map.empty

      expectDirect [dep] graph
      expectDeps [dep] graph
      expectEdges [] graph

    it "should resolve url for bitbucket source" $ do
      let graph =
            buildGraph $
              ShardYmlContent
                (Just $ Map.fromList [(PackageName "a", gitSourceWithBranch bitBucketOrigin "hotfix")])
                Nothing

      let dep =
            Dependency
              GitType
              "https://bitbucket.com/bitbucket_username/repo.git"
              (Just $ CEq "hotfix")
              []
              [EnvProduction]
              Map.empty

      expectDirect [dep] graph
      expectDeps [dep] graph
      expectEdges [] graph

    it "should not graph path dependency" $ do
      let graph =
            buildGraph $
              ShardYmlContent
                ( Just $
                    Map.fromList
                      [ (PackageName "g_path_a", pathSource "./../local-path")
                      ]
                )
                ( Just $
                    Map.fromList
                      [ (PackageName "g_path_b", pathSource "./../local-path")
                      ]
                )
      expectDirect [] graph
      expectDeps [] graph
      expectEdges [] graph

    it "should build graph" $ do
      let graph =
            buildGraph $
              ShardYmlContent
                ( Just $
                    Map.fromList
                      [ (PackageName "g_a", gitSourceWithCommit gitOrigin "ccc")
                      , (PackageName "g_b", gitSourceWithTag gitOrigin "v1.2.3")
                      , (PackageName "g_c", gitSourceWithVersion gitOrigin ">= 2.3.1")
                      , (PackageName "g_d", gitSourceOnly gitOrigin)
                      , (PackageName "g_e", gitSourceWithBranch gitOrigin "hotfix")
                      ]
                )
                ( Just $
                    Map.fromList
                      [ (PackageName "g_e", gitSourceWithBranch gitOrigin "automated-workflow")
                      ]
                )

      let deps =
            [ newGitOriginDep (Just $ CEq "ccc") EnvProduction
            , newGitOriginDep (Just $ CEq "v1.2.3") EnvProduction
            , newGitOriginDep (Just $ CEq ">= 2.3.1") EnvProduction
            , newGitOriginDep Nothing EnvProduction
            , newGitOriginDep (Just $ CEq "hotfix") EnvProduction
            , newGitOriginDep (Just $ CEq "automated-workflow") EnvDevelopment
            ]

      expectDirect deps graph
      expectDeps deps graph
      expectEdges [] graph
