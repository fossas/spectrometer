## Manually specifying dependencies

FOSSA offers a way to manually upload dependencies provided we support the dependency type. Manually specifying dependencies is very helpful in the event your package manager is unsupported or you are using a custom and nonstandard dependency management solution.

The FOSSA CLI will automatically read a `fossa-deps.yml` or a `fossa-deps.json` file in the root directory (usually the current working directory) when `fossa analyze` is run and parse dependencies from it. These dependencies will be added to the dependencies that are normally found when `fossa analyze` is run in the directory.

> Tip: Use a script to generate this file before running `fossa analyze` to keep your results updated.

To manually specify a dependency, you must provide the package type, package name, and optionally a package version, under the `referenced-dependencies` array, as shown here:

```yaml
referenced-dependencies:
- type: gem
  name: iron
- type: pypi
  name: Django
  version: 2.1.7
```

The `name` and `type` fields are required and specify the name of the dependency and where to find it. The `version` field is optional and specifies the preferred version of dependency.

Supported dependency types:

- `cargo` - Rust dependencies that are typically found at [crates.io](https://crates.io/).
- `carthage` - Dependencies as specified by the [Carthage](https://github.com/Carthage/Carthage) package manager.
- `composer` - Dependencies specified by the PHP package manager [Composer](https://getcomposer.org/), which are located on [Packagist](https://packagist.org/).
- `cpan` - Dependencies located on the [CPAN package manager](https://www.cpan.org/).
- `gem` - Dependencies which can be found at [RubyGems.org](https://rubygems.org/).
- `git` - Github projects (which appear as dependencies in many package managers). Specified as the full github repository `https://github.com/fossas/spectrometer`.
- `go` - Golang specific dependency. Many golang dependencies are located on Github, but there are some which look like the following `go.mongodb.org/mongo-driver` that have custom golang URLs.
- `hackage` - Haskell dependencies found at [Hackage](https://hackage.haskell.org/).
- `hex` - Erlang and Elixir dependencies that are found at [Hex.pm](https://hex.pm/).
- `maven` - Maven dependencies that can be found at many different sources. Specified as `name: javax.xml.bind:jaxb-api` where the convention is `groupId:artifactId`.
- `npm` - Javascript dependencies found at [npmjs.com](https://www.npmjs.com/).
- `nuget` - .NET dependencies found at [NuGet.org](https://www.nuget.org/).
- `paket` - .NET dependencies found at [fsprojects.github.io/Paket/](https://fsprojects.github.io/Paket/).
- `pub` - Dart dependencies found at [pub.dev](https://www.pub.dev/).
- `pypi` - Python dependencies that are typically found at [Pypi.org](https://pypi.org/).
- `cocoapods` - Swift and Objective-C dependencies found at [Cocoapods.org](https://cocoapods.org/).
- `url` - The URL type allows you to specify only the download location of an archive (e.g.: `.zip`, .`tar.gz`, etc.) in the `name` field and the FOSSA backend will attempt to download and scan it. Example for a github source dependency `https://github.com/fossas/spectrometer/archive/refs/tags/v2.7.2.tar.gz`. The `version` field will be silently ignored for `url` type dependencies.
