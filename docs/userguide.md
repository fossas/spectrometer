
# Spectrometer User Guide

1. [Quick Start](#quick-start)
2. [Supported Languages](#supported-languages)
3. Commands
    - [`fossa analyze`](#fossa-analyze)
        - [Specifying FOSSA project details](#specifying-fossa-project-details)
        - [Printing results without uploading to FOSSA](#printing-results-without-uploading-to-fossa)
        - [Running in a specific directory](#running-in-a-specific-directory)
        - [Scanning archive contents](#scanning-archive-contents)
        - [Manually specifying dependencies](#manually-specifying-dependencies)
    - [`fossa test`](#fossa-test)
        - [Specifying a timeout](#specifying-a-timeout)
        - [Print issues as json](#print-issues-as-json)
4. [Common FOSSA Project Flags](#common-fossa-project-flags)
5. [Frequently-Asked Questions](#frequently-asked-questions)
    - [`fossa analyze`: Why wasn't my project found?](#fossa-analyze-why-wasnt-my-project-found)
    - [When are you adding support for (some buildtool/language)](#when-are-you-adding-support-for-some-buildtoollanguage)

## Quick Start

Spectrometer is a tool that requires minimal configuration: usually, only a FOSSA API key is required.

If you do not have an API key, please check the [FOSSA documentation](https://docs.fossa.com/docs/api-reference) for instructions on generating an API key.

### Configure your API key

```sh
export FOSSA_API_KEY=abcdef123456
```

### Run Analysis

This runs dependency analysis in the current directory, uploading results to FOSSA

```sh
fossa analyze
```

### Check for FOSSA scan results

```sh
fossa test
```

For additional commands and command flags, use `--help`:

```sh
fossa --help
fossa analyze --help
# etc
```

## Supported Languages

### clojure

- [leiningen](quickreference/leiningen.md)

### erlang

- [rebar3](quickreference/rebar3.md)

### golang

- [gomodules (`go mod`)](quickreference/gomodules.md)
- [dep](quickreference/godep.md)
- [glide](quickreference/glide.md)

### haskell

- [cabal](quickreference/cabal.md)
- [stack](quickreference/stack.md)

### java

- [maven](quickreference/maven.md)
- [gradle](quickreference/gradle.md)

### javascript/typescript

- [yarn](quickreference/yarn.md)
- [npm](quickreference/npm.md)

### .NET

- [nuget](quickreference/nuget.md)

### objective-c

- [carthage](quickreference/carthage.md)
- [cocoapods](quickreference/cocoapods.md)

### python

- [conda](quickreference/conda.md)
- [`requirements.txt`/`setup.py`](quickreference/setuptools.md)
- [pipenv](quickreference/pipenv.md)

### ruby

- [bundler](quickreference/bundler.md)

### rust

- [cargo](quickreference/cargo.md)

### scala

- [sbt](quickreference/sbt.md)
- [gradle](quickreference/gradle.md)
- [maven](quickreference/maven.md)

### swift

- [carthage](quickreference/carthage.md)
- [cocoapods](quickreference/cocoapods.md)

## `fossa analyze`

By default, the analyze command:

- looks for projects in the current directory (and recursively in subdirectories)
- extracts dependency graphs from those projects
- infers a project name, branch, and revision for the project (from git or svn)
- uploads the dependency graphs to FOSSA

For supported command-line flags, use `fossa analyze --help`

### Specifying FOSSA project details

In addition to the [usual FOSSA project flags](#common-fossa-project-flags) supported by all commands, the analyze command supports the following FOSSA-project-related flags:

| Name | Description |
| ---- | ----------- |
| `--title 'some title'` | Set the title of the FOSSA project |
| `--branch 'some branch'` | Override the detected FOSSA project branch |
| `--project-url 'https://example.com'` | Add a URL to the FOSSA project |
| `--jira-project-key 'some-key'` | Add a Jira project key to the FOSSA project |
| `--link 'https://example.com'` | Attach a link to the current FOSSA build |
| `--team 'some team'` | Specify a team within your FOSSA organization |
| `--policy 'some policy'` | Assign a specific FOSSA policy to this project |

### Printing results without uploading to FOSSA

The `--output` flag can be used to print projects and dependency graph information to stdout, rather than uploading to FOSSA

```sh
fossa analyze --output
```

### Running in a specific directory

```sh
fossa analyze path/to/directory
```

### Scanning archive contents

With the `--unpack-archives` flag present, we unpack discovered archives to a temporary directory for dependency analysis. We recursively unpack archives-in-archives.

```sh
fossa analyze --unpack-archives
```

We support the following archive formats:

- `.zip`
- `.tar`
- `.tar.gz`
- `.jar`
- `.rpm`

## Manually specifying dependencies

FOSSA offers a way to manually upload dependencies provided we support the dependency type. Manually specifying dependencies is very helpful in the event your package manager is unsupported or you are using a custom and nonstandard dependency management solution.

The FOSSA CLI will automatically read `fossa-deps.yml` in the root directory (usually the current working directory) when `fossa analyze` is run and parse dependencies from it.

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
- `gem` - Dependencies which can be found at [RubyGems.org](https://rubygems.org/).
- `git` - Github projects (which appear as dependencies in many package managers). Specified as the full github repository `https://github.com/fossas/spectrometer`.
- `go` - Golang specific dependency. Many golang dependencies are located on Github, but there are some which look like the following `go.mongodb.org/mongo-driver` that have custom golang URLs.
- `hackage` - Haskell dependencies found at [Hackage](https://hackage.haskell.org/).
- `hex` - Erlang and Elixir dependencies that are found at [Hex.pm](https://hex.pm/).
- `maven` - Maven dependencies that can be found at many different sources. Specified as `name: javax.xml.bind:jaxb-api` where the convention is `groupId:artifactId`.
- `npm` - Javascript dependencies found at [npmjs.com](https://www.npmjs.com/).
- `nuget` - .NET dependencies found at [NuGet.org](https://www.nuget.org/).
- `pypi` - Python dependencies that are typically found at [Pypi.org](https://pypi.org/).
- `cocoapods` - Swift and Objective-C dependencies found at [Cocoapods.org](https://cocoapods.org/).
- `url` - The URL type allows you to specify only the download location of an archive (e.g.: `.zip`, .`tar.gz`, etc.) in the `name` field and the FOSSA backend will attempt to download and scan it. Example for a github source dependency `https://github.com/fossas/spectrometer/archive/refs/tags/v2.7.2.tar.gz`. The `version` field will be silently ignored for `url` type dependencies.

### User-defined dependencies

FOSSA supports users that have dependencies that can't be automatically discovered or identified, by offering the ability to define new dependencies.

To do this, you must supply the name, version, and license of the dependency.  This creates a stub package which requires no source code or linkage to any other system, but still acts as a normal dependency in other areas of FOSSA, like reports and the dependency views.
You may also supply a description and/or url, but both are optional.  Note that these fields reference the dependency itself, and do not reference the parent project (the one at the current analysis directory), or the individual versions of the dependency.

```yaml
custom-dependencies:
# Custom dependencies need name, version, and license
- name: foo
  version: 1.2.3
  license: "MIT or Apache-2.0"
# You can also provide a description and/or homepage
- name: foo-wrapper
  version: 1.2.3
  license: MIT
  metadata:
    homepage: https://www.foowrapper.com/about
    description: Provides foo and a helpful interface around foo-like tasks.
```

### Remote dependencies

FOSSA also supports dependencies that can't be automatically discovered or identified, but the user has a remote URL that points to an archive where the dependency is hosted.

To do this, you must supply the name, version, and url of the dependency. The FOSSA backend will attempt to download and scan the archive hosted at this remote location. Example for a github source dependency: `https://github.com/fossas/spectrometer/archive/refs/tags/v2.7.2.tar.gz`. You may also supply a description that will be included inside your report, however this is optional.

```yaml
remote-dependencies:
# Remote dependencies need name, version, and url
- name: foo
  version: 1.2.3
  url: https://www.fooarchive.tar.gz

# You can also provide a description and/or homepage
- name: foo-wrapper
  version: 1.2.3
  url: https://www.foowrapper.tar.gz
  metadata:
    description: Provides foo and a helpful interface around foo-like tasks.
    homepage: https://www.foowrapper-home.com
```

### Errors in the `fossa-deps` file

The `fossa-deps` scanner tries to report clear error messages when fields are missing, incorrect, or invalid.  For example:

```yaml
referenced-dependencies:
- type: pypi
  name: flask
  version: "2.0.1"
  license: MIT  # Error!  "license" is only allowed for custom-dependencies

custom-dependencies:
- type: custom  # Error!  "type" is only allowed for referenced-dependencies
  name: mydep
  version: "3.14.15"
  license: GPL-3.0

remote-dependencies:
- name: mydep
  version: "3.14.15"
  license: GPL-3.0 # Error! "license" is only allowed for custom-dependencies
```

This would return an error with a message explaining what went wrong, and where.  However, we don't check for everything (yet!):

```yaml
referenced-dependencies:
- type: cargo
  name: bitflags
  some-unexpected-field: hello  # Has no effect, will be considered an error in future versions.
```

The `fossa-deps` scanner also requires at least one valid dependency if the file exists.  This prevents the file from being created with the wrong array names and us silently ignoring them.

If you see an error message that isn't clear, file an issue in this repository!  Clear error messages are a priority for us, and we want to know where we're lacking.

## `fossa test`

The test command checks whether the most-recent scan of your FOSSA project raised license-policy or vulnerability issues. This command is usually run immediately after `fossa analyze`

- If there are issues, it prints them to stdout and fails with an exit code of 1
- If there are no issues, it prints nothing and succeeds with an exit code of 0

`fossa test` supports the [Common FOSSA Project Flags](#common-fossa-project-flags) supported by all commands

### Specifying a timeout

By default, `fossa test` waits a maximum of 10 minutes for issue scan results. To override the default timeout, use, e.g.:

```sh
fossa test --timeout 60
```

Where `60` is the maximum number of seconds to wait for issue scan results.

### Print issues as JSON

By default, `fossa test` displays issues in a human-readable format. To instead print issues as JSON, use:

```sh
fossa test --json
```

## `fossa report`

The report command downloads a report of the most-recent scan of your FOSSA project. This command is usually run immedately after `fossa analyze` or `fossa test`

The report type **must** be specified to successfully run, for example:

```sh
fossa report attribution
```

`fossa report` supports the [Common FOSSA Project Flags](#common-fossa-project-flags) supported by all commands.

### Report types

- `fossa report attribution` - A report that contains information about your dependencies and their authors. For more info about attributions, check the [FOSSA docs page illustrating the topic](https://docs.fossa.com/docs/generating-reports).

### Specifying a report timeout

By default, `fossa report` waits a maximum of 10 minutes for report contents. To override the default timeout, use, e.g.:

```sh
fossa report attribution --timeout 60
```

Where `60` is the maximum number of seconds to wait for the report to be downloaded.

### Print report as JSON

By default, `fossa report` displays issues in a human-readable format. To instead print issues as JSON, use:

```sh
fossa report attribtion --json
```

*NOTE: Currently, text reports are not supported, and the report will be*
*printed as JSON.  It is recommended to use the `--json` flag anyway, since*
*the behavior of the command without the flag will change in the future.*

## Common FOSSA Project Flags

All `fossa` commands support the following FOSSA-project-related flags:

| Name | Description |
| ---- | ----------- |
| `--project 'some project'` | Override the detected project name |
| `--revision 'some revision'` | Override the detected project revision |
| `--fossa-api-key 'my-api-key'` | An alternative to using the `FOSSA_API_KEY` environment variable to specify a FOSSA API key |
| `--endpoint 'https://example.com'` | Override the FOSSA API server base URL |

## Frequently-Asked Questions

### `fossa analyze`: Why wasn't my project found?

If your project wasn't found, make sure you meet the requirements in the [relevant language/build-tool's quick reference](#supported-languages)

If your project meets the requirements, it's very likely `fossa analyze` found your project, but dependency analysis failed. To show the failure reason, use the `--debug` flag:

```sh
fossa analyze --debug
```

Note that the output is likely to be very noisy: our project discovery process is very lenient, and can produce many false-positives. False-positive projects almost always fail during the dependency analysis step, so we don't show analysis failures by default.

If your project wasn't in the `--debug` output, or you believe you've encountered a bug, please [file a bug](https://github.com/fossas/spectrometer/issues/new).

In your bug report, please include:

- relevant package manifest files (e.g., `pom.xml` or `package.json`)
- the output of `fossa analyze --debug`

### When are you adding support for (some buildtool/language)?

If we don't support your choice of language/buildtool, please [open an issue](https://github.com/fossas/spectrometer/issues/new) to express interest
