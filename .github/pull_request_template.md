# Overview

_Provide an overview of this change. Describe the intent of this change, and how it implements that intent._

## Acceptance criteria

_What is this PR trying to accomplish?_

## Testing plan

_How did you validate that this PR works? How did you check that the acceptance criteria were fulfilled?_

_This section should list reproduction steps that a reviewer can run through (and provide any needed test cases)._

## Risks

_Highlight any areas that you're unsure of, or want reviewers to pay particular attention to._

## References

_List any referenced GitHub issues. If PR references tickets in other systems (e.g. Zendesk), they should probably be mirrored as a GitHub Issue._

_Make sure to use keywords to link this PR to GitHub issues ([GitHub Docs](https://docs.github.com/en/github/managing-your-work-on-github/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword))._

_Example:_ Closes org/repo#123.

## Checklist

- [ ] I added tests for this PR's change (or explained in the PR description why tests are not viable).
- [ ] If this PR introduced a user-visible change, I added documentation into `docs/`.
- [ ] If this PR introduced a user-visible change, I updated `Changelog.md`. If this PR did not mark a release, I added my changes into an `# Unreleased` section at the top.
- [ ] If this PR changed the shape of `.fossa.yml` or `fossa-deps.{json,yaml,yml}`, I updated `docs/references/files/*.schema.json`. (You may also need to update these if you have added or removed new dependency type (e.g. `pip`) or analysis target type (e.g. `poetry`).)
- [ ] If this PR added Template Haskell that needs to re-run when external files change (e.g. for embedding files), I added those external files to the list of `extra-source-files` in `spectrometer.cabal`.
- [ ] I linked this PR to any referenced GitHub issues, if they exist.
