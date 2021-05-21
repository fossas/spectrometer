# Hacking

## Quickstart

Use [ghcup][ghcup] to install the `cabal` cli tool and the ghc version we're using:

```sh
$ ghcup install ghc 8.10
$ ghcup set ghc 8.10
$ ghcup install cabal 
$ ghcup set cabal
$ cabal update
$ cabal build
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
| [haskell-language-server][hls] | LSP server for haskell projects |
| [hlint][hlint] | A linting + hints tool for haskell code. It provides really useful suggestions |
| [ormolu][ormolu] | A haskell source code formatter |
| [fourmolu][fourmolu] | A forked version of ormolu that we are evaluating |

### Installing haskell-language-server

In VSCode: Install the "Haskell Language Server" (`haskell.haskell`) plugin in VSCode. In the
settings there's a `Language Server Haskell: Hie Variant` option, which you can
set to `haskell-language-server`

## Linting

Install [hlint][hlint].  Run `hlint /path/to/file-or-directory`.  You can also configure the haskell language server to run hlint.
See [haskell-anguage-server][hls] for configuration instructions.

`hlint` errors are usually required changes in pull requests, but we do not currently run `hlint` in CI, as there are a few outstanding lint errors that have not yet been fixed.
You do not need to enforce that `hlint` passes to submit a PR, but it does help greatly, for both the author and reviewer.

## Formatting

Currently, we do not have a standardized formatting solution.  We have been using `ormolu`, but are now evaluating `fourmolu`, as it provides some configuration options that we want to take advantage of.

For now, you can run `ormolu --mode inplace /path/to/file1 /path/to/file2`.  Shell globs and file-finders are recommended for formatting lots of files.  We do not require formatting yet, and when we do, we will ensure that it is present in CI.

## Docs

| name | description |
| ---- | ----------- |
| [hoogle][hoogle] | Search for type signatures or symbols |
| [hackage][hackage] | Package repository; can be used to browse invdividual package docs ("haddocks") |

If on macOS, [dash](https://kapeli.com/dash) is a great tool that allows for downloading searchable package haddocks

On linux, you can use [zeal](https://zealdocs.org/).  (Currently there is an issue with building third-party docsets, if you discover a solution to get e.g.: `aeson` docs in `zeal`, please file an issue or submit a PR to fix these docs.)

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

[fourmolu]: https://github.com/fourmolu/fourmolu
[ghcup]: https://www.haskell.org/ghcup
[hackage]: https://hackage.haskell.org/
[hlint]: https://github.com/ndmitchell/hlint
[hls]: https://github.com/haskell/haskell-language-server
[hoogle]: https://hoogle.haskell.org/
[ormolu]: https://github.com/tweag/ormolu
