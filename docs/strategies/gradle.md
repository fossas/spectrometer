# Gradle

[Gradle](https://gradle.org/) is a polyglot tool mostly used by JVM projects. It's popular with Java and Android projects.

Gradle users generally specify their builds using a `build.gradle` file, written in Groovy. These builds are specified as programs that must be dynamically evaluated.

|           |                                          |
| --------- | ---------------------------------------- |
| :warning: | This strategy requires dynamic analysis. |

<!-- omit in toc -->
## Table of contents

- [Concepts](#concepts)
  - [Subprojects and configurations](#subprojects-and-configurations)
  - [Dependencies and artifacts](#dependencies-and-artifacts)
  - [Gradle wrappers](#gradle-wrappers)
- [Running Gradle](#running-gradle)
- [Discovery](#discovery)
- [Tactics](#tactics)
  - [Tactic selection](#tactic-selection)
  - [Gradle build plugin](#gradle-build-plugin)
  - [Parsing `gradle :dependencies`](#parsing-gradle-dependencies)
- [Debugging an integration](#debugging-an-integration)
  - [Determining whether Gradle targets are detected](#determining-whether-gradle-targets-are-detected)
  - [Manually checking Gradle dependency results](#manually-checking-gradle-dependency-results)
  - [Debugging the "Gradle build plugin" tactic](#debugging-the-gradle-build-plugin-tactic)
- [Manually specifying Gradle dependencies](#manually-specifying-gradle-dependencies)

## Concepts

### Subprojects and configurations

TODO: finish

### Dependencies and artifacts

TODO: finish

### Gradle wrappers

TODO: finish

## Running Gradle

This strategy requires dynamic analysis in its discovery phase (not just in the analysis phase). This is because we need to execute Gradle in order to list subprojects and evaluate `build.gradle` files.

When executing Gradle for an analysis target at directory `ANALYSIS_TARGET_DIR`, the CLI will prefer (in order):

1. `$ANALYSIS_TARGET_DIR/gradlew`
2. `$ANALYSIS_TARGET_DIR/gradlew.bat`
3. `gradle` (from `$PATH`)

For more details, see [Gradle wrappers](#gradle-wrappers).

In this documentation below, for brevity, we'll always refer to the selected tool as `gradle`.

## Discovery

This strategy discovers analysis targets by looking for files in the folder being analyzed whose names start with `build.gradle`. This matches both `build.gradle` as well as `build.gradle.kts` and build files in other Gradle-supported languages (`build.gradle.*`).

It then executes `gradle projects` in the directory where the build file is found to get a list of subprojects for this Gradle build. These subprojects are used to create the analysis targets for this Gradle build.

If there are no subprojects, a analysis target is created that analyzes the root project. Otherwise, a set of analysis targets is created: one for each Gradle subproject.

## Tactics

### Tactic selection

This strategy selects tactics by trying them in preference order and using the results of the first tactic that succeeds.

The order of tactics for this strategy is:

1. Gradle build plugin
2. Parsing `gradle :dependencies` (not yet implemented)

### Gradle build plugin

|                    |                                                                                          |
| ------------------ | ---------------------------------------------------------------------------------------- |
| :heavy_check_mark: | This tactic reports dependencies for all subprojects.                                    |
| :heavy_check_mark: | This tactic provides a graph for subproject dependencies.                                |
| :warning:          | This tactic requires dynamic analysis.                                                   | . |
| :warning:          | This tactic only reports dependencies in the `default` configuration of each subproject. |

This tactic runs a Gradle [init script](https://docs.gradle.org/current/userguide/init_scripts.html) to output the dependencies in each Gradle subproject. Mechanically, this tactic:

1. Unpacks our init script to a temporary directory.
2. Invokes the init script with `gradle jsonDeps -Ipath/to/init.gradle`.
3. Parses the JSON output of the init script.

This init script is implemented [here](https://github.com/fossas/spectrometer/blob/master/scripts/jsondeps.gradle) and bundled into the CLI during compilation.

The script works by iterating through configurations, resolving their dependencies, and then serializing those dependencies into JSON.

Currently, this script only reports dependencies in the `default` configuration of each subproject.

### Parsing `gradle :dependencies`

|           |                                        |
| --------- | -------------------------------------- |
| :x:       | This tactic is not yet implemented.    |
| :warning: | This tactic requires dynamic analysis. |

This not-yet-implemented tactic will execute `gradle $SUBPROJECT:dependencies` for each analysis target, and parse the tool's output.

## Debugging an integration

### Determining whether Gradle targets are detected

To determine whether the CLI is properly detecting your Gradle project, run `fossa list-targets`. The output of this command is a list of analysis targets, in the format `type@path`.

<!-- TODO: is there a guide for `fossa list-targets` I can reference here? -->

For each of your Gradle subprojects, you should see a `gradle@PATH_TO_GRADLE_SUBPROJECT` target in the list of analysis targets.

If you _don't_ see this, one of two things is likely happening:

1. Your Gradle project does not have a `build.gradle` file. This is an unsupported configuration.
2. `gradle projects` is failing to execute. Make sure that a Gradle wrapper is accessible (see [Running Gradle](#running-gradle)), and make sure `gradle projects` runs successfully.

### Manually checking Gradle dependency results

To manually verify the correctness of the CLI's results, run `gradle :dependencies` in your root project, and `gradle $SUBPROJECT:dependencies` for each subproject.

The CLI should produce a graph of dependencies that's a union of the dependencies of each subproject.

If your CLI run uploads versions that differ from the output of `gradle $SUBPROJECT:dependencies`, check to make sure that the subproject dependency's version is the actual version resolved across the entire Gradle build. Different Gradle subprojects may select different dependency versions when resolved independently, but will select a single resolved version when the build is resolved as a whole.

If you'd like to make a bug report about incorrect dependencies, make sure to include the list of incorrect dependencies, as well as the commands you ran to obtain that list.

### Debugging the "Gradle build plugin" tactic

The Gradle build plugin is a Gradle [init script](https://docs.gradle.org/current/userguide/init_scripts.html) implemented [here](../../scripts/jsondeps.gradle).

If this tactic doesn't appear to be working (e.g. is giving you incorrect dependencies or is missing dependencies), you can run the init script directly using:

```
gradle -I$PATH_TO_SCRIPT $SUBPROJECT1:jsonDeps $SUBPROJECT2:jsonDeps ...
```

For example, for a Gradle build with subprojects `foo` and `bar` and the script extracted to `/tmp/jsondeps.gradle`, you should run (from within the build file's working directory):

```
gradle -I/tmp/jsondeps.gradle foo:jsonDeps bar:jsonDeps
```

Providing this output with a bug report will help us debug issues with the analysis.

## Manually specifying Gradle dependencies

If the CLI doesn't natively integrate with your build tool (e.g. if you have a homegrown tool), and your build tool uses Gradle dependencies, you can still manually add Gradle dependencies to an uploaded build. This feature is generally known as [manual dependencies](../userguide.md#manually-specifying-dependencies).

Gradle in particular actually uploads _Maven_ dependencies, since most Gradle builds use Gradle's Maven interoperability to get dependencies from Maven repositories.

An example configuration file looks like:

```yaml
# fossa-deps.yml
referenced-dependencies:
- type: maven
  name: javax.xml.bind:jaxb-api
  version: 1.0.0
```

Notice that the `name` field follows Maven conventions: `groupId:artifactId`.

For more details, see the [manual dependencies](../userguide.md#manually-specifying-dependencies) documentation.
