# Crystal - Shards

Shards is crystal dependency manager, accompanied usual with language. It manages dependencies for crystal projects.

## Project Discovery

Find the first file named `shard.yml`

## Analysis: shard.yml

we parse `shard.yml` file for direct dependencies. When faced with version constraint (e.g. >= 2.3.0), we resolve to latest version matching constraint. 

## Limitations

- Only direct dependencies are reported. 
- Path dependencies are not supported.

## Example 

1. Create shard.yml file for your project with `shard init`
2. Update your shard file with relevant dependencies. Example shown below.

```yaml
name: appname
version: 1.2.3
crystal: '>= 0.35.0'

authors:
- user <user@example.com>

description: |
  example description

dependencies:
dependencies:
  pg:
    github: will/crystal-pg
    version: "~> 0.5"
  pkgC:
    path: ./../local-path-c/

development_dependencies:
  webmock:
    github: manastech/webmock.cr

```
3. Run `fossa analyze` on root of the project.

## References

- [Shards Code](https://github.com/crystal-lang/shards)
- [Shard.yml Spec](https://github.com/crystal-lang/shards/blob/master/docs/shard.yml.adoc)
