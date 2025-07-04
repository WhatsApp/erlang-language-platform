---
sidebar_position: 5
---

# The `.elp.toml` Configuration File

A `.elp.toml` configuration file can be added to a project's root directory to
customize the behaviour of ELP for a given project.

The file can also be used as **an absolute marker for the root of an ELP
project**, If one is found - and it contains all the required information -, ELP
will not perform any upward auto-discovery for the structure of a project.

## A Sample `.elp.toml` Configuration File

Here is an example of a full `.elp.toml` file. All sections are optional. The
[build_info](#build-info) can be used to mark the root of a project (via the
`build_info` property). The available configuration sections are described
below.

```toml
[build_info]
file = "my_hand_crafted_build_info.json"

[eqwalizer]
enable_all = true
max_tasks = 32
ignore_modules = ["very_big_generated_module"]

[buck]
enabled = false

[rebar]
profile = "test"
```

## Configuration Sections

### \[build_info\] {#build-info}

This section is used to configure project discovery.

| Key  | Type                                | Description                                                                                                                                                                                                                        |
| ---- | ----------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| file | String                              | Path to a JSON file describing the project. This is only honoured if `buck.enabled` is `false` or missing. The format of the JSON file is described in the [build_info.json](custom-project.md#the-build_infojson-format) section. |
| apps | String or Array of Strings/App Data | A string pattern or a sequence of string patterns or app data used to generate the applications for the `build_info.json` configuration. Unused if `file` is specified. See below for examples.                                    |
| deps | String or Array of Strings/App Data | A string pattern or a sequence of string patterns or app data used to generate the dependencies for the `build_info.json` configuration. Unused if `file` is specified. See below for examples.                                    |

The `apps` and `deps` fields can contain a string pattern or a sequence of
string patterns. Each pattern can use wildcards. E.g.

    [build_info]
    apps = ["some_apps/*", "more_apps/*"]
    deps = "deps/*"

Each pattern will be expanded, behind the scenes, to an _app_, as described in
the [build_info.json](custom-project.md#the-build_infojson-format) section. Each
app will look something like:

    {
        "name": "app_name",
        "dir": "path/to/app",                         // Relative to project root
        "src_dirs": ["path/to/src", ...],             // Relative to app dir, defaults to ["src"]
        "extra_src_dirs": ["path/to/extra_src", ...], // Relative to app dir, defaults to []
        "ebin": "path/to/ebin",                       // Relative to app dir, defaults to "ebin"
        "include_dirs": ["include", ...],             // Relative to app dir, defaults to []
        "macros": ["MACRO", ...],                     // Defaults to []
    }

It is possible to override these values for a given app. For example:

    [build_info]
    apps = [ "some_apps/*",
             "more_apps/*",
             {name = "special_app", dir = "some_apps/special_app", "src_dirs" = ["src", "more_src"]}
             ]
    deps = "deps/*"

The order in which the entries are specified is important, in the sense that
"the last entry wins". In the above example, the `special_app` in
`some_apps/special_app` will contain an extra source directory, while all the
other apps in the same `some_apps` directory will default to `src` only.

### \[eqwalizer\] {#eqwalizer}

ELP is integrated with the [eqWAlizer](https://github.com/whatsapp/eqwalizer)
type checker. The integration can be configured via this section.

:::info

By default eqWAlizer is enabled on all non-test modules, including generated modules.
This can be overriden per module via the following attributes:

- `-eqwalizer(ignore).` Opt-out module unconditionally
- `-typing([eqwalizer]).` Opt-in unconditionally, even for test modules

:::

| Key            | Type           | Description                                                                                                                                          |
| -------------- | -------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------- |
| enabled_all    | Boolean        | Enable eqwalizer for all modules by default, but still honours the module-specific overrides listed above and the ignore_modules list.               |
| max_tasks      | Integer        | Max number of parallel eqWAlizer tasks, defaults to 4 (eqWAlizer instances are memory intensive). This only applies to using eqWAlizer from the CLI. |
| ignore_modules | List of String | Disable eqwalizer for individual modules from the config.                                                                                            |

### \[buck\] {#buck}

Configure the interaction between ELP and the [Buck2](https://buck2.build/)
build tool. See [this presentation](https://youtu.be/4ALgsBqNBhQ) for details
about Erlang support for Buck2.

| Key     | Type    | Description                      |
| ------- | ------- | -------------------------------- |
| enabled | Boolean | Discover the project using Buck2 |

:::warning

The github version is not built with buck2 support enabled. This will change
soon, once we tweak the tests.

:::

### \[rebar\] {#rebar}

Configure ELP for [rebar3](https://rebar3.org/)-based projects.

| Key     | Type   | Description                                                                                                                                | Default |
| ------- | ------ | ------------------------------------------------------------------------------------------------------------------------------------------ | ------- |
| profile | String | The `rebar3` profile to use for project discovery. Only used if the `file` property is specified in the [build_info](#build-info) section. | test    |
