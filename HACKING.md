# Hacking

## Quickstart

Use [ghcup][ghcup] to install the `cabal` cli tool and the ghc version we're using:

```sh
$ ghcup install-cabal
$ ghcup install 8.8
$ ghcup set 8.8
```

### Building

In the base directory, run `cabal build`

### Running tests

In the base directory, run `cabal test`

## Tools

| name | description |
| ---- | ----------- |
| [ghcup][ghcup] | Used to manage installed versions of ghc and cabal-install |
| ghc | The haskell compiler (installed via ghcup) |
| cabal-install | The package manager we use (installed via ghcup) |
| [haskell-language-server][hls] | LSP server for haskell projects (see below for installation instructions). Includes a formatter |
| hlint | A linting + hints tool for haskell code. It provides really useful suggestions |

### Installing haskell-language-server

```sh
git clone --recursive https://github.com/haskell/haskell-language-server.git
cd haskell-language-server
cabal update
cabal install --installdir=$HOME/.cabal/bin --overwrite-policy=always
```

- VSCode: Install Install the "Haskell Language Server" plugin in VSCode. In the
settings there's a `Language Server Haskell: Hie Variant` option, which you can
set to `haskell-language-server`

### Installing hlint

```sh
cabal update
cabal install hlint --installdir=$HOME/.cabal/bin --overwrite-policy=always
```

- Run it with `hlint path/to/dir/or/file`

## Docs

| name | description |
| ---- | ----------- |
| [hoogle][hoogle] | Search for type signatures or symbols |
| [hackage][hackage] | Package repository; can be used to browse invdividual package docs ("haddocks") |

If on macOS, [dash](https://kapeli.com/dash) is a great tool that allows for downloading searchable package haddocks

## Cheatsheets

### Cabal cheatsheet

| command | description |
| ------- | ----------- |
| `cabal repl` | opens the ghci repl on the project |
| `cabal build` | build spectrometer |
| `cabal test` | build + run tests |
| `cabal run binary-name -- arg1 arg2` | build + run an executable named `binary-name`, and with args `arg1` `arg2` |

### GHCI cheatsheet

Use `cabal repl` to open ghci.

| command | description |
| ------- | ----------- |
| `:r`/`:reload` | reload the project |
| `:t`/`:type <symbol>` | query the type of a symbol |
| `:i`/`:info <symbol>` | query info about a symbol -- docs, where it was defined, etc |
| `:l`/`:load <Module.Name>` | load a specific file into the repl |

[ghcup]: https://gitlab.haskell.org/haskell/ghcup
[hls]: https://github.com/haskell/haskell-language-server
[hoogle]: https://hoogle.haskell.org/
[hackage]: https://hackage.haskell.org/
