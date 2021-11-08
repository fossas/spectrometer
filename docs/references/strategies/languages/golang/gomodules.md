# Golang Modules

Golang 1.11 has first-class support for "modules", which is now the preferred
way to do dependency management.

## Project Discovery

Find all files named `go.mod`

## Analysis: golist

Discovery: find go.mod files

We run `go list -m -json all`, which produces, e.g.,:

```json
{
        "Path": "example.com/foo/bar",
        "Main": true,
        "Dir": "/Users/example/Codes/golang/simple",
        "GoMod": "/Users/example/Codes/golang/simple/go.mod",
        "GoVersion": "1.16"
}
{
        "Path": "github.com/kr/pretty",
        "Version": "v0.1.0",
        "Time": "2018-05-06T08:33:45Z",
        "Indirect": true,
        "Dir": "/Users/example/go/pkg/mod/github.com/kr/pretty@v0.1.0",
        "GoMod": "/Users/example/go/pkg/mod/cache/download/github.com/kr/pretty/@v/v0.1.0.mod"
}
{
        "Path": "github.com/kr/pty",
        "Version": "v1.1.1",
        "Time": "2018-01-13T18:08:13Z",
        "Indirect": true,
        "GoMod": "/Users/example/go/pkg/mod/cache/download/github.com/kr/pty/@v/v1.1.1.mod"
}
{
        "Path": "github.com/kr/text",
        "Version": "v0.1.0",
        "Time": "2018-05-06T08:24:08Z",
        "Indirect": true,
        "Dir": "/Users/example/go/pkg/mod/github.com/kr/text@v0.1.0",
        "GoMod": "/Users/example/go/pkg/mod/cache/download/github.com/kr/text/@v/v0.1.0.mod"
}
{
        "Path": "gopkg.in/check.v1",
        "Version": "v1.0.0-20180628173108-788fd7840127",
        "Time": "2018-06-28T17:31:08Z",
        "Indirect": true,
        "Dir": "/Users/example/go/pkg/mod/gopkg.in/check.v1@v1.0.0-20180628173108-788fd7840127",
        "GoMod": "/Users/example/go/pkg/mod/cache/download/gopkg.in/check.v1/@v/v1.0.0-20180628173108-788fd7840127.mod"
}
{
        "Path": "gopkg.in/yaml.v3",
        "Version": "v3.0.0-20210107192922-496545a6307b",
        "Time": "2021-01-07T19:29:22Z",
        "Dir": "/Users/example/go/pkg/mod/gopkg.in/yaml.v3@v3.0.0-20210107192922-496545a6307b",
        "GoMod": "/Users/example/go/pkg/mod/cache/download/gopkg.in/yaml.v3/@v/v3.0.0-20210107192922-496545a6307b.mod"
}
```

- To infer direct dependencies, we filter out any module, which has `Main` field with value of true, and `Indirect` field with value of true.
- To infer transitive dependencies, we execute `go list -json all`, and parse it's output for `Imports`, `ImportPath`, `Module`, `Standard` data, and fill in the transitive dependencies.

For package dependencies that aren't using gomodules, a pseudo-version (`v0.0.0-TIMESTAMP-COMMITID`) is present instead. We use the commit ID as the version.

## Analysis: gomod

We parse the go.mod file, which looks something like:

```
module our/package/path

require (
    github.com/example/one v1.2.3
    github.com/example/two v2.3.4
)

replace github.com/example/two => github.com/example/other v2.0.0
```

where:

- `replace` rewrites `require`s. In this example, our requires resolve to
  `[github.com/example/one v1.2.3, github.com/example/other v2.0.0]`
