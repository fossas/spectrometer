### License scanning local dependencies

Fossa offers the ability to license scan your code directly. This is used primarily if a package manager is not yet supported or if you are vendoring dependencies. Using the license scanning feature will allow you to capture the licenses for dependencies that may otherwise be missed from normal fossa analysis that relies on package manager information.

In order to specify a file path, modify your `fossa-deps.yml` file and add a `vendored-dependencies` section like the following:

```yml
# Example full `fossa.deps.yml` file.
referenced-dependencies:
- type: gem
  name: rubyXL
  version: 3.4.16

vendored-dependencies:
- name: Django
  path: vendor/Django-3.4.16.zip # path can be either a file or a folder.
  version: 3.4.16 # revision will be set to the MD5 hash of the filepath if left unspecified.
```

> Note: License scanning currently operates by uploading the files at the specified path to a secure S3 bucket. All files that do not contain licenses are then removed after 2 weeks.

We also support json-formatted dependencies:

```json
{
  "referenced-dependencies": [
    {
      "type": "gem",
      "name": "iron"
    }, {
      "type": "pypi",
      "name": "Django",
      "version": "2.1.7"
    }
  ],
  "custom-dependencies": [
    {
      "name": "foo",
      "version": "1.2.3",
      "license": "MIT"
    }, {
      "name": "foo-wrapper",
      "version": "1.0.2",
      "license": "MIT or Apache-2.0",
      "metadata": {
        "description": "Provides a help wrapper for foo-related tasks",
        "homepage": "https://foo-project.org/homepage"
      }
    }
  ],
  "vendored-dependencies": [
    {
      "name": "lodash",
      "path": "lodash-4.17.21"
    }, {
      "name": "winston",
      "path": "vendor/winston.tar.gz",
      "version": "5.0.0-alpha"
    }
  ],
  "remote-dependencies": [
    {
      "name": "foo-url",
      "version": "1.2.3",
      "url": "www.foo.tar.gz",
      "metadata": {
        "description": "foo archive",
        "homepage": "https://foo-url.org/homepage"
      }
    }
  ]
}
```
