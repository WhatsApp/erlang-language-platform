---
sidebar_position: 4
---

# The `.elp.toml` Configuration File

A `.elp.toml` configuration file can be added to a project's root directory to
customize the behaviour of ELP for a given project.

The file can also be used as **an absolute marker for the root of an ELP
project**, If one is found - and it contains all the required information -, ELP
will not perform any upward auto-discovery for the structure of a project.

## A Sample `.elp.toml` Configuration File

Here is an example of a full `.elp.toml` file. All sections are optional. The
[build_info](#empty-section) can be used to mark the root of a project (via
the `build_info` property). The available configuration sections are described
below.

```toml
[build_info]
file = "my_hand_crafted_build_info.json"

[eqwalizer]
enable_all = true
max_tasks = 32

[buck]
enabled = false

[rebar]
profile = "test"
```

## Configuration Sections

### \[build\_info\]

This section is used to configure project discovery.

| Key        | Type   | Description                                                                                                                                                                                            |
| ---------- | ------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| file | String | Path to a JSON file describing the project. This is only honoured if `buck.enabled` is `false` or missing. The format of the JSON file is described in the [build_info.json](#the-build_infojson-format) section. |
| apps | String | A string pattern used to generate the applications for the `build_info.json` configuration. Unused if `file` is specified. E.g. `apps/*`|
| deps | String | A string pattern used to generate the dependencies for the `build_info.json` configuration. Unused if `file` is specified. E.g. `deps/*`|

### \[eqwalizer\]

ELP is integrated with the [eqWAlizer](https://github.com/whatsapp/eqwalizer)
type checker. The integration can be configured via this section.

:::info

By default eqWAlizer is enabled on all non-test modules. It is also disabled for
modules containing the `@generated` keyword within their first 2000 characters.
This can be overriden per module via the following attributes:

- `-eqwalizer(ignore).` Opt-out module unconditionally
- `-typing([eqwalizer]).` Opt-in unconditionally, even for test modules

:::

| Key         | Type    | Description                                                                                                |
| ----------- | ------- | ---------------------------------------------------------------------------------------------------------- |
| enabled_all | Boolean | Disable eqwalizer for all modules by default, but still honours the module-specific overrides listed above |
| max_tasks   | Integer | Max number of parallel eqWAlizer tasks, defaults to 4 (eqWAlizer instances are memory intensive). This only applies to using eqWAlizer from the CLI.          |

### \[buck\]

Configure the interaction between ELP and the [Buck2](https://buck2.build/)
build tool. See [this presentation](https://youtu.be/4ALgsBqNBhQ) for details
about Erlang support for Buck2.

| Key     | Type    | Description                      |
| ------- | ------- | -------------------------------- |
| enabled | Boolean | Discover the project using Buck2 |

:::warning

The github version is not built with buck2 support enabled. This will
change soon, once we tweak the tests.

:::

### \[rebar\]

Configure ELP for [rebar3](https://rebar3.org/)-based projects.

| Key     | Type   | Description                                                                                                                                    | Default |
| ------- | ------ | ---------------------------------------------------------------------------------------------------------------------------------------------- | ------- |
| profile | String | The `rebar3` profile to use for project discovery. Only used if the `file` property is specified in the [build_info](#build-info) section.     | test    |
