# Spectrometer

[![Build](https://github.com/fossas/spectrometer/actions/workflows/build.yml/badge.svg)](https://github.com/fossas/spectrometer/actions/workflows/build.yml)
[![Dependency scan](https://github.com/fossas/spectrometer/actions/workflows/dependency-scan.yml/badge.svg)](https://github.com/fossas/spectrometer/actions/workflows/dependency-scan.yml)
[![FOSSA Status](https://app.fossa.com/api/projects/custom%2B1%2Fgithub.com%2Ffossas%2Fspectrometer.svg?type=shield)](https://app.fossa.com/projects/custom%2B1%2Fgithub.com%2Ffossas%2Fspectrometer?ref=badge_shield)

Spectrometer is a zero-configuration polyglot dependency analysis tool. You can point Spectrometer at any codebase or build, and it will automatically detect dependencies being used by your project.

<!-- TODO: Flesh out X and Y. Ideally, link to reference documentation. -->
Spectrometer currently supports automatic dependency analysis for X+ languages and Y+ build tools. It also has limited support for vendored dependency detection, container scanning, and system dependency detection. These features are still a work in progress. Our goal is to make Spectrometer a single, universal tool for all kinds of dependency analysis.

Spectrometer integrates deeply with [FOSSA](https://fossa.com) for dependency analysis, license scanning, vulnerability scanning, attribution report generation, and more. You can also use Spectrometer as a standalone dependency analysis tool, although some features (such as vendored dependency detection) may not work without a FOSSA integration.

## Table of Contents

1. [Installation](#installation)
2. [Getting Started](#getting-started)
3. [User Manual](#user-guide)
4. [Reporting Issues](#reporting-issues)
5. [Contributing](#contributing)

## Installation

### Using the install script

Spectrometer provides an install script that downloads the latest release from GitHub Releases for your computer's architecture. You can see the source code and flags at [`install.sh`](https://github.com/fossas/spectrometer/blob/master/install.sh) for Mac and Linux or [`install.ps1`](https://github.com/fossas/spectrometer/blob/master/install.ps1) for Windows.

**NOTE:** You may need to add the downloaded executable to your `$PATH`. The installer script will output the installed path of the executable. You can also use `-b` to pick the installation directory when using `install.sh` (see [the `install.sh` source code](https://github.com/fossas/spectrometer/blob/master/install.sh) for details).

#### macOS or 64-bit Linux

```bash
curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/spectrometer/master/install.sh | bash
```

#### macOS with Apple M1 Silicon

We do not currently support ARM as a target architecture. You can work around this on M1 Mac devices using the M1's x86_64 emulation.

```bash
arch -x86_64 /bin/bash -c "$(curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/spectrometer/master/install.sh)"
```

#### Windows with Powershell

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force; iex  ((New-Object System.Net.WebClient).DownloadString('https://raw.githubusercontent.com/fossas/spectrometer/master/install.ps1'))
```

### Installing manually

You can install Spectrometer releases manually by downloading the latest release from [GitHub Releases](https://github.com/fossas/spectrometer/releases) and extracting the binary to your `$PATH`.

## Getting Started

### Integrating your project with FOSSA

#### TL;DR

```sh
curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/spectrometer/master/install.sh | bash

export FOSSA_API_KEY=XXXX

cd $MY_PROJECT_DIR
fossa analyze
```

#### Installing Spectrometer

Follow [the installation instructions]() above to install Spectrometer. Once installed, you should have a new binary named `fossa` available on your `$PATH`.

#### Generating an API key

To get started with integrating your project into FOSSA, you'll need to [generate an API key](). You'll get this API key from the FOSSA web application ([app.fossa.com]()). You can follow the instructions [here]() to generate your API key.

Once you have your API key:

```sh
export FOSSA_API_KEY=XXXX # Use your API key here.
```

#### Running an analysis

Now we can run an analysis. To run an analysis, all you need to do is navigate to your project's directory and run `fossa analyze`.

**NOTE:** While `fossa` will try its best to report available results for any kind of project, you'll get the best results by running in a directory with a working project build. A working build lets us integrate directly with your build tool to identify dependencies, instead of trying to infer dependencies from your source code.

```sh
$ cd $MY_PROJECT_DIR # Use your actual project location here.

$ fossa analyze
[ INFO] Using project name: `https://github.com/fossas/spectrometer`
[ INFO] Using revision: `09ca72e398bb32747b27c0f43731678fa42c3c26`
[ INFO] Using branch: `No branch (detached HEAD)`
[ INFO] ============================================================

      View FOSSA Report:
      https://app.fossa.com/projects/custom+1%2fgithub.com%2ffossas%2fspectrometer/refs/branch/master/09ca72e398bb32747b27c0f43731678fa42c3c26

  ============================================================
```

#### Viewing your results

Once an analysis has been uploaded, you can view your results in the FOSSA web application. You can see your analysis by using the link provided as output by `fossa analyze`, or by navigating to your project and revision in the FOSSA web application.

#### What next?

Now that your analysis is complete, there are a couple things you might want to do after an initial integration:

- **Double-check your results.** Some analysis methods may produce partial or unexpected results depending on what information was available when you ran the analysis. If something seems wrong, [our debugging guide]() can help you diagnose and debug your integration.
- **Scan for issues and generate a compliance report.** TODO: finish
- **Set up FOSSA in your CI.** (`fossa test` + GH webhooks)

### Using Spectrometer as a standalone tool

if you'd like to script

some features won't work

```sh
```

Note that Spectrometer's `--output` format is **not considered stable**. While we will try our best to maintain backwards compatibility, this output format may change without warning between releases.

## User Manual

For most users, Spectrometer should work out-of-the-box without any configuration. Just get an API key, run `fossa analyze`, and view your results in the FOSSA web application.

Users who need more advanced customizations or features should see the [User Manual](). Some common topics of interest include:

- Vendored dependencies.
- Config file.
- Debugging.

## Reporting Issues

TODO: --- reword

If you are experiencing an issue related to the results on the FOSSA
website/dashboard, please contact [support@fossa.com](mailto:support@fossa.com)

Issues specific to Spectrometer should be filed through the [Github issues
page](https://github.com/fossas/spectrometer/issues/new).

Please include the following in your bug report:

- Steps to reproduce your issue
- Relevant project manifest files (e.g., `pom.xml` or `package.json`)

## Contributing

For development documentation (still WIP, but not empty), see our [Development
Docs Homepage](devdocs/index.md).
