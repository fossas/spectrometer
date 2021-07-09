# `.fossa.yml`

The fossa configuration file, `.fossa.yml`, is an optional file located at the root of a project that can be used to configure project settings. 

The following example is a configuration file with all available fields filled out. All fields except for `version` are optional. Configuration file versions 1 and 2 were used for CLI versions prior to 2.0.0.


```yaml
version: 3

server: https://app.fossa.com
apiKey: a1b2c3

project:
  id: github.com/fossas/fossa-cli
  name: fossa-cli
  team: cli-team
  policy: custom-cli-policy
  link: fossa.com
  url: github.com/fossas/spectrometer
  jiraProjectKey: jira-key
  releaseGroup:
    name: release-group-name
    release: 123-release-candidate

revision:
  commit: "12345"
  branch: master
```


## Fields

### `version:`
Specifies the version of configuration file. Versions 1 and 2 were used by CLI versions up until CLI 2.0.0 and are no longer supported. Version 3 is the current supported version for FOSSA CLI v2.

### `server:`
Sets the endpoint that the CLI will send requests to. This field should only be modified if your FOSSA account lives on a different server than app.fossa.com. This is most commonly needed with on-premise instances of FOSSA.

Default: `https://app.fossa.com`

### `apiKey:`
Sets the [FOSSA API key](https://docs.fossa.com/docs/api-reference#api-tokens) that is required for accessing the FOSSA API and uploading data (e.g. `fossa analyze`) or retrieving information (e.g. `fossa test`) about a project.

> Note: FOSSA strongly recommends setting the API key as an environment variable and NOT in the configuration file for security purposes.

### `project:`
The project fields allow you to configure settings for the project you are interacting with through the FOSSA API.

> Note: `name`, `team`, `policy`, `link`, `url`, and `jiraProjectKey` can only be set when creating a project (running `fossa analyze` for the first time).

#### `id:`
The project ID defines a unique ID that the FOSSA API will use to reference this project. The project ID can be found in the UI on the project settings page listed as the "Project Locator" underneath the "Project Title" setting.

By default, the CLI will look for a `.git/config` file and set the ID to the project's remote "origin" url. We will also attempt to run `svn info` if your project is using Apache Subversion. If your project is not using any VCS (Version control system), the ID will be set to the name of the project's directory.

> Note: A project's ID cannot be modified after a project is created. If you change the ID, you will be interacting with a different project. If the new ID does not exist, a new project will be created for it.

#### `name:`
The name field sets the projects visible name in the FOSSA dashboard. By default, this will be set to the project's ID.

#### `team:`
The name of the team in your FOSSA organization to associate this project with.

#### `policy:`
The name of the policy in your FOSSA organization to associate this project with.

#### `link:`
An external link that will appear in the FOSSA UI for this specific project.

#### `url:`
The URL of your project that will appear in FOSSA. This URL is intended to be the URL to the repository of this project.

#### `jiraProjectKey:`
The Jira Project Key to associate with your project for improved issue triage. Refer to the [FOSSA docs](https://docs.fossa.com/docs/atlassian-jira#linking-fossa-projects-to-jira-projects) for more information.

#### `releaseGroup:`
The `name:` and `release:` of the release group's release to add your project to in the FOSSA dashboard.

If you choose to associate a project with a release group, you **must** supply both name and release.

### `revision:`
The revision fields are used to help FOSSA differentiate between one upload for a project and another, just as GitHub uses commit hashes and branch names.

#### `commit:`
The revision is used to identify a specific scan for a project (determined by project.id). This is intended to be used identically to how Git treats commit hashes. 

By default, the CLI will parse the current HEAD state in the `.git` directory and use the commit hash of the HEAD branch. For SVN, we will run `svn info` and gather a revision. If no VCS exists, we will set the revision to the current unix timestamp.

#### `branch:`
The project branch is an optional setting used for organizing project revisions in the FOSSA UI. The branch field intended to function similar to how Git defines a branch. By default, the CLI attempts to find the projects current branch from the `.git/config` file.

## FAQ

### Why are some configuration settings (name, team, policy, etc.) ignored by the FOSSA API after a project has already been created?

The purpose of allowing a user to set `policy`, `team`, and other settings in the configuration file is to make it easy for users to share configuration files within their teams when creating many different projects. If these configuration settings were allowed to modify a project every time they were set on the CLI they could disrupt anyone managing the project in the FOSSA UI. Example: I change a project from Team A to Team B in the FOSSA UI. The project is then scanned nightly in a CI environment and my UI team change is reverted. This behavior would be very difficult for someone managing the project only in the FOSSA UI to diagnose and fix.