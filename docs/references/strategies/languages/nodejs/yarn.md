# Yarn

Yarn is a spiritual successor to the npm cli. Yarn automatically updates the
`yarn.lock` file when the yarn CLI is used to modify the dependencies present in
the project. The `yarn.lock` file contains information about a dependency's
transitive dependencies, its location, and its resolved version.

## Project Discovery

Find all files named `yarn.lock`, as well as any associated `package.json` files.

## Analysis

yarn.lock has its own bespoke format. Dependencies typically look something
like:

```yarn-lock
"@babel/code-frame@^7.0.0", "@babel/code-frame@^7.5.5":
  version "7.5.5"
  resolved "https://registry.yarnpkg.com/@babel/code-frame/-/code-frame-7.5.5.tgz#bc0782f6d69f7b7d49531219699b988f669a8f9d"
  integrity sha512-27d4lZoomVyo51VegxI20xZPuSHusqbQag/ztrBC7wegWoQ1nLREPVSKSW8byhTlzTKyNE4ifaTA6lCp7JjpFw==
  dependencies:
    "@babel/highlight" "^7.0.0"
```

where:

- `resolved` is the exact location of the dependency
- `version` is a pinned dependency
- the section "keys" contain constraints used by other packages to refer to this
  dependency
- `dependencies` is a list of constraints for dependencies of this package

We also support yarn V2, which uses a similar, though mechanically different
schema, but uses standard YAML rather than a bespoke format.

As of _v2.19.x_, we also support yarn workspaces.  In practice, this means that
the `package.json` files used to build the `yarn.lock` file are also checked,
and the knowledge of both is combined to form a (usually) complete picture of
the full graph of dependencies.
