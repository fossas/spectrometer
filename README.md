# Spectrometer

[![Build](https://github.com/fossas/spectrometer/actions/workflows/build.yml/badge.svg)](https://github.com/fossas/spectrometer/actions/workflows/build.yml)
[![Dependency scan](https://github.com/fossas/spectrometer/actions/workflows/dependency-scan.yml/badge.svg)](https://github.com/fossas/spectrometer/actions/workflows/dependency-scan.yml)
[![FOSSA Status](https://app.fossa.com/api/projects/custom%2B1%2Fgithub.com%2Ffossas%2Fspectrometer.svg?type=shield)](https://app.fossa.com/projects/custom%2B1%2Fgithub.com%2Ffossas%2Fspectrometer?ref=badge_shield)

Spectrometer is a zero-configuration polyglot dependency analysis tool. You can point Spectrometer at any codebase or build, and it will automatically detect dependencies being used by your project.

<!-- TODO: Flesh out X and Y. Ideally, link to reference documentation. -->
Spectrometer currently supports automatic dependency analysis for X languages and Y build tools. It also has limited support for vendored dependency detection, container scanning, and system dependency detection. These features are still a work in progress. Our goal is to make Spectrometer a one-stop shop for all kinds of dependency analysis.

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

Generate API key

Download `fossa`

Run `fossa analyze`

Debugging? see here

### Using Spectrometer as a standalone tool

## User Manual

## Using Spectrometer

See the [User Guide](docs/userguide.md) for detailed instructions.

Usually, this is sufficient:

``` sh
# configure api key
export FOSSA_API_KEY=your-api-key-goes-here

# run dependency analysis in the current
# directory, uploading results to FOSSA
fossa analyze

# check for FOSSA license- and vulnerability-scan results
fossa test
```


## Reporting Issues

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
