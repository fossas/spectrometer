# Spectrometer

Spectrometer is a polyglot dependency analysis tool, designed to identify and
report dependency trees for many languages and package managers.  See the
[docs](docs/strategies.md) for a non-exhaustive list of supported
languages/managers.

```sh
me@mydesk:~/myproject$ fossa2 analyze -o
# Dependency analysis output
```

## Table of Contents

1. [Installation](#installation)
1. [Basic Usage](#basic-usage)
3. [Supported Strategies](#supported-languagesmanagers)
4. [Contributing/Building From Source](#contributingbuilding-from-source)

## Installation

### MacOS (Darwin) or Linux amd64:
```bash
curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/spectrometer/master/install.sh | bash
```

### Windows with Powershell:
```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force; iex  ((New-Object System.Net.WebClient).DownloadString('https://raw.githubusercontent.com/fossas/spectrometer/master/install.ps1'))
```

**NOTE**: In order to use the executable, you may need to add it to your PATH.
The installer will report the installation path of the executable to make this
easier.

## Basic Usage

The tool requires little-to-no configuration to run on its own.

```sh
fossa2 analyze --output # Analyze the current working directory
# OR
fossa2 analyze --output --basedir /path/to/project  # Analyze a specific directory.
```

The `--output` flag tells the analyzer not to connect to a FOSSA server,
instead printing out the analysis result to the console.

While we're on the subject, you can (and should!) connect your output directly
to your FOSSA server!  You can do this by simply supplying the FOSSA API key
from your server ([more info](https://docs.fossa.com/docs/api-reference)) and
using it as shown:

```sh
export FOSSA_API_KEY=your-key-goes-here
fossa2 analyze
# OR
fossa2 analyze --fossa-api-key your-key-goes-here
```

This not only lets you re-examine the results later, but allows you to check
your output against your preset policies.

**NOTE** If leaked, your FOSSA API key can grant an attacker access to your FOSSA
projects, and should be kept secret.  For this reason, we recommend supplying
the API key with the environment variable, especially in a shared environment
like a CI/CD server.

## Supported Languages/Managers

A non-exhaustive list of supported languages and managers can be found
[here](docs/strategies.md).  This list is a work-in-progress, as some existing 
strategies are not yet documented, but are implemented.

## Building From Source

### Building

Use [ghcup](https://gitlab.haskell.org/haskell/ghcup) to install the `cabal` cli tool and the ghc version we're using:

```sh
$ ghcup install-cabal
$ ghcup install 8.8
$ ghcup set 8.8
```

In the base directory, run `cabal build`

### Running

```sh
$ cabal run fossa -- analyze -d path/to/basedir/ -o
```

This will produce analysis results on stdout

### Testing

Configure the project to enable tests
```sh
cabal configure --enable-tests
```

Run the tests:
```
cabal test
```
