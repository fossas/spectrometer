# Perl Analysis

| Strategy           | Direct Deps        | Deep Deps          | Edges | Classifies Dev Dependencies |
| ------------------ | ------------------ | ------------------ | ----- | --------------------------- |
| `*META.{yml, json} | :white_check_mark: | :white_check_mark: | :x:   | :white_check_mark:          |

## Project Discovery

Find a file named `MYMETA.json` or `MYMETA.yml` or `META.json` or `META.yml`.

## Analysis

1. Parse `MYMETA.{yml, json}` or `META.{yml, json}` to identify dependencies.

## Limitation

- Dependency required for `runtime` only will be reported.
- Reported analysis will not have any edges.

## Example 

1. Build using: `perl Makefile.PL`. When you do this, you'll see that `MYMETA.yml` and `MYMETA.json` are generated.
4. Execute `fossa analyze -o` on the project to print analyzed dependency graphing (this will not upload any analysis to any endpoint)

## FAQ

### How do I *only perform analysis* for the perl?

You can explicitly specify an analysis target in `.fossa.yml` file. The example below will exclude all analysis targets except for the composer. 

```yaml
# .fossa.yml 

version: 3
targets:
  only:
    - type: perl
```