module Yarn.V2.ResolversSpec (
  spec,
) where

import Data.Foldable (for_)
import Data.Text
import Strategy.Yarn.V2.Lockfile
import Strategy.Yarn.V2.Resolvers
import Test.Hspec

spec :: Spec
spec = do
  testResolver
    "Workspace"
    workspaceResolver
    [ (Locator Nothing "unused" "workspace:.", WorkspacePackage ".")
    , (Locator Nothing "unused" "workspace:bar", WorkspacePackage "bar")
    , (Locator Nothing "unused" "workspace:../baz", WorkspacePackage "../baz")
    ]

  testResolver
    "Npm"
    npmResolver
    [ -- without a scope
      (Locator Nothing "packagename" "npm:1.0.0", NpmPackage Nothing "packagename" "1.0.0")
    , -- with a scope
      (Locator (Just "withscope") "packagename" "npm:1.0.0", NpmPackage (Just "withscope") "packagename" "1.0.0")
    ]

  testResolver
    "Git"
    gitResolver
    [ (Locator Nothing "unused" "https://example.com/foo.git#commit=abcdef", GitPackage "https://example.com/foo.git" "abcdef")
    , -- a case where there are several keys after #
      (Locator Nothing "unused" "https://example.com/foo.git#branch=something&commit=abcdef&otherkey=somethingelse", GitPackage "https://example.com/foo.git" "abcdef")
    ]

  testResolver
    "Tar"
    tarResolver
    [ -- https url, .tar.gz
      (Locator Nothing "unused" "https://link.to/tarball.tar.gz", TarPackage "https://link.to/tarball.tar.gz")
    , -- http url, .tgz
      (Locator Nothing "unused" "http://link.to/tarball.tar.gz", TarPackage "http://link.to/tarball.tar.gz")
    , -- https url, .tgz
      (Locator Nothing "unused" "https://link.to/tarball.tgz", TarPackage "https://link.to/tarball.tgz")
    , -- awkward input
      (Locator Nothing "unused" "https://link.to/tarball..tgz?foo=bar", TarPackage "https://link.to/tarball..tgz?foo=bar")
    ]

  testUnsupportedResolver "File" fileResolver "file:" FilePackage
  testUnsupportedResolver "Link" linkResolver "link:" LinkPackage
  testUnsupportedResolver "Portal" portalResolver "portal:" PortalPackage
  testUnsupportedResolver "Exec" execResolver "exec:" ExecPackage
  testUnsupportedResolver "Patch" patchResolver "patch:" PatchPackage

testResolver ::
  -- | Name of the resolver
  String ->
  Resolver ->
  -- | A list of (locator, expected package resolution) pairs
  [(Locator, Package)] ->
  Spec
testResolver name resolver supported =
  describe (name <> "Resolver") $ do
    it "Should work for supported locators" $ do
      for_ supported $ \(locator, result) -> do
        resolverSupportsLocator resolver locator `shouldBe` True
        resolverLocatorToPackage resolver locator `shouldBe` Right result

testUnsupportedResolver ::
  -- | Name of the resolver
  String ->
  Resolver ->
  -- | Protocol prefix
  Text ->
  -- | Constructor for packages
  (Text -> Package) ->
  Spec
testUnsupportedResolver name resolver protocol constructor =
  testResolver
    name
    resolver
    [(Locator Nothing "unused" (protocol <> "somepackage"), constructor "somepackage")]
