# Working with Test and Development Dependency

FOSSA CLI, by default, does not include the unused dependency in analysis. This is expected for scenarios in which, you have a dependency for development, testing, and other purposes - which ultimately are not used in your runtime. FOSSA CLI can infer dependency's inclusion for runtime or distribution by package manager's manifest file. When we are unable to infer if the dependency is only used in runtime, we choose the conservative approach and include them in our analysis reporting. 

CLI's language support documentation (in [strategy's section](./../references/strategies/README.md)), includes whether an analysis of a particular project will by default unused dependency.

## Example 

For the following `package.json` file:

```json
{
    "name": "example-project",
    "dependencies": {
        "pngjs": "6.0.0",
        "once": "1.4.0"
    },
    "devDependencies": {
        "mocha": "*"
    }
}
```

### When running `fossa analyze`:

We will report the dependency tree as follows:

```tree
.
 |-npm+once$1.4.0 (direct)
 | \-npm+wrappy$1 (transitive)
 \-npm+pngjs$6.0.0 (direct)
```

Note that, we are not reporting "mocha" dependency or any other transitive dependency of mocha. This is intended behavior, as aforementioned, FOSSA CLI, by default only report dependency which is used in your runtime or distribution.

### How to include all dependencies?

You can use the `--include-unused-deps` flag to explicitly include all _discovered_ dependencies found. This will include all unused dependencies (development, configuration, etc.)

You can run analysis like this:
```bash
fossa analyze --include-unused-deps
```

With the flag, it will report the following dependency graph now (notice the inclusion of mocha and its dependencies)

```tree
.
 |-npm+mocha$9.1.3 (direct)
 | |-npm+@ungap/promise-all-settled$1.1.2
 | |-npm+ansi-colors$4.1.1
 | |-npm+browser-stdout$1.3.1
 | |-npm+chokidar$3.5.2
 | | |-npm+anymatch$~3.1.2
 | | |-npm+braces$~3.0.2
 | | |-npm+fsevents$~2.3.2
 | | |-npm+glob-parent$~5.1.2
 | | |-npm+is-binary-path$~2.1.0
 | | |-npm+is-glob$~4.0.1
 | | |-npm+normalize-path$~3.0.0
 | | \-npm+readdirp$~3.6.0
 | |-npm+debug$4.3.2
 | | \-npm+ms$2.1.2
 | |-npm+diff$5.0.0
 | |-npm+escape-string-regexp$4.0.0
 | |-npm+find-up$5.0.0
 | | |-npm+locate-path$^6.0.0
 | | \-npm+path-exists$^4.0.0
 | |-npm+glob$7.1.7
 | | |-npm+fs.realpath$^1.0.0
 | | |-npm+inflight$^1.0.4
 | | |-npm+inherits$2
 | | |-npm+minimatch$^3.0.4
 | | |-npm+once$^1.3.0
 | | \-npm+path-is-absolute$^1.0.0
 | |-npm+growl$1.10.5
 | |-npm+he$1.2.0
 | |-npm+js-yaml$4.1.0
 | | \-npm+argparse$^2.0.1
 | |-npm+log-symbols$4.1.0
 | | |-npm+chalk$^4.1.0
 | | \-npm+is-unicode-supported$^0.1.0
 | |-npm+minimatch$3.0.4
 | | \-npm+brace-expansion$^1.1.7
 | |-npm+ms$2.1.3
 | |-npm+nanoid$3.1.25
 | |-npm+serialize-javascript$6.0.0
 | | \-npm+randombytes$^2.1.0
 | |-npm+strip-json-comments$3.1.1
 | |-npm+supports-color$8.1.1
 | | \-npm+has-flag$^4.0.0
 | |-npm+which$2.0.2
 | | \-npm+isexe$^2.0.0
 | |-npm+workerpool$6.1.5
 | |-npm+yargs$16.2.0
 | | |-npm+cliui$^7.0.2
 | | |-npm+escalade$^3.1.1
 | | |-npm+get-caller-file$^2.0.5
 | | |-npm+require-directory$^2.1.1
 | | |-npm+string-width$^4.2.0
 | | |-npm+y18n$^5.0.5
 | | \-npm+yargs-parser$^20.2.2
 | |-npm+yargs-parser$20.2.4
 | \-npm+yargs-unparser$2.0.0
 |  |-npm+camelcase$^6.0.0
 |  |-npm+decamelize$^4.0.0
 |  |-npm+flat$^5.0.2
 |  \-npm+is-plain-obj$^2.1.0
 |-npm+once$1.4.0 (direct)
 | \-npm+wrappy$1
 \-npm+pngjs$6.0.0 (direct)
```