# Npm (cli)

The npm cli is the canonical package manager for nodejs projects.

## Project Discovery

`npmlist`: Find all directories containing a file named `package.json` (package manifests)

`npmlock`: Find all files named `package-lock.json`, not descending into
directories named `node_modules`

## Analysis: npmlist

Running `npm ls --json` outputs a tree of all the installed dependencies, their
version, and their transitive dependencies. Adding `--production` or
`--development` allows us to determine the dependencies which are used only in
production or in development. Running `npm ls --json --production` and merging
the results with `npm ls --json --development` provides us with all of the
information we are interested in about npm dependencies.

## Analysis: npmlock

The package lock json file is generated when npm modifies `node_modules` or
`package.json` and describes the exact dependency tree generated.

> Note: In old versions of npm, package-lock.json was only modified when dependencies were installed.

This dependency tree contains information about a dependency's version, its transitive dependencies, the URL where the dependency is located at, and whether or not the dependency is used as a development dependency or not. The transitive dependency information is listed in an unintuitvie way. Under each dependency there may be two fields, `requires` and `dependencies` as in the following example:

```json
    "babel-code-frame": {
      "version": "6.26.0",
      "resolved": "https://registry.npmjs.org/babel-code-frame/-/babel-code-frame-6.26.0.tgz",
      "integrity": "sha1-Y/1D99weO7fONZR9uP42mj9Yx0s=",
      "requires": {
        "chalk": "1.1.3",
        "esutils": "2.0.3",
        "js-tokens": "3.0.2"
      },
      "dependencies": {
        "ansi-styles": {
          "version": "2.2.1",
          "resolved": "https://registry.npmjs.org/ansi-styles/-/ansi-styles-2.2.1.tgz",
          "integrity": "sha1-tDLdM1i2NM914eRmQ2gkBTPB3b4="
        },
        "js-tokens": {
          "version": "3.0.2",
          "resolved": "https://registry.npmjs.org/js-tokens/-/js-tokens-3.0.2.tgz",
          "integrity": "sha1-mGbfOVECEw449/mWvOtlRDIJwls="
        }
      }
    }
```

The `requires` field signifies all of the dependencies that are needed by the dependency in order to properly function.

The `dependencies` field signifies all of the dependencies included in `babel-code-frame`'s `node_modules` folder within the top level `node_modules` folder. Notice that these dependencies are not always included in the `requires` section.

> Note: `npm-shrinkwrap.json` is an identically formatted file that can be used for [publishing packages](https://docs.npmjs.com/cli/shrinkwrap).
