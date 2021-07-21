# Gradle

[Gradle](https://gradle.org/) is a polyglot tool mostly used by JVM projects. It's popular with Java and Android projects.

Gradle users generally specify their builds using a `build.gradle` file, written in Groovy. These build configurations are programs that must be dynamically evaluated.

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

When executing Gradle for an analysis target at directory `$ANALYSIS_TARGET_DIR`, the CLI will prefer (in order):

1. `$ANALYSIS_TARGET_DIR/gradlew`
2. `$ANALYSIS_TARGET_DIR/gradlew.bat`
3. `gradle` (from `$PATH`)

For more details, see [Gradle wrappers](#gradle-wrappers).

In this documentation below, for brevity, we'll always refer to the selected tool as `gradle`.

## Discovery

This strategy discovers analysis targets by looking for files whose names start with `build.gradle` in the folder being analyzed. This matches both `build.gradle` as well as `build.gradle.kts` and build configurations in other Gradle-supported languages (`build.gradle.*`).

It then executes `gradle projects` to get a list of subprojects for this Gradle analysis target. If there are no subprojects, the analysis target will analyze the root project. Otherwise, each subproject is considered a separate analysis target.

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

Currently, this script only reports dependencies in the `default` configuration of each subproject.

### Parsing `gradle :dependencies`

|           |                                        |
| --------- | -------------------------------------- |
| :x:       | This tactic is not yet implemented.    |
| :warning: | This tactic requires dynamic analysis. |

This not-yet-implemented tactic will execute `gradle $SUBPROJECT:dependencies` for each analysis target.

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

TODO: finish

### Debugging the "Gradle build plugin" tactic

TODO: finish

## Manually specifying Gradle dependencies

TODO: finish
