### Custom dependencies

FOSSA supports users that have dependencies that can't be automatically discovered or identified, by offering the ability to define new dependencies.

To do this, you must supply the name, version, and license of the dependency.  This creates a stub package which requires no source code or linkage to any other system, but still acts as a normal dependency in other areas of FOSSA, like reports and the dependency views.
You may also supply a description and/or url, but both are optional.  Note that these fields reference the dependency itself, and do not reference the parent project (the one at the current analysis directory), or the individual versions of the dependency.

```yaml
custom-dependencies:
# Custom dependencies need name, version, and license
- name: foo
  version: 1.2.3
  license: "MIT or Apache-2.0"
# You can also provide a description and/or homepage. These values populate metadata fields in reports in the FOSSA web UI.
- name: foo-wrapper
  version: 1.2.3
  license: MIT
  metadata:
    homepage: https://www.foowrapper.com/about
    description: Provides foo and a helpful interface around foo-like tasks.
```

### Remote dependencies

FOSSA also supports dependencies that can't be automatically discovered or identified, but where the user has a URL where FOSSA can download the source code of the dependency.

To specify a remote dependency, you must provide the name, version, and download URL of the dependency. The FOSSA backend will attempt to download and scan any source code contained in an archive hosted at this URL.

For example, for a dependency released on a GitHub release, your URL might look like: `https://github.com/fossas/spectrometer/archive/refs/tags/v2.7.2.tar.gz`.

You can also optionally add metadata fields ("description" and "homepage") to populate these fields in the FOSSA web UI (these fields can be displayed when generating reports).

```yaml
remote-dependencies:
# Remote dependencies need name, version, and url
- name: foo
  version: 1.2.3
  url: https://www.fooarchive.tar.gz
# You can also provide a description and/or homepage. These values populate metadata fields in reports in the FOSSA web UI.
- name: foo-wrapper
  version: 1.2.3
  url: https://www.foowrapper.tar.gz
  metadata:
    description: Provides foo and a helpful interface around foo-like tasks.
    homepage: https://www.foowrapper-home.com
```
