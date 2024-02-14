---
sidebar_position: 4
---

# Project Discovery and Configuration

## Configuration for use with rebar3 projects

1. Use OTP 25
2. Download the `elp` binary for your system from https://github.com/WhatsApp/erlang-language-platform/releases

    > On Mac you will probably get the following message when trying to run the executable the first time: "elp cannot be opened because the developer cannot be verified.".
    To solve this, go to Preferences > Security and Privacy and add an exception. Alternatively, you can build elp from source.

3. Add `eqwalizer_support` dependency and `eqwalizer_rebar3` plugin
   to your rebar3 project definition (see below)
4. From the project directory run:
  - `elp eqwalize <module>` to type-check a single module
  - `elp eqwalize-all` to type-check all `src` modules in the project


Adding `eqwalizer_support` and `eqwalizer_rebar3`:

```
{deps, [
  {eqwalizer_support,
    {git_subdir,
        "https://github.com/whatsapp/eqwalizer.git",
        {branch, "main"},
        "eqwalizer_support"}}
]}.

{project_plugins, [
  {eqwalizer_rebar3,
    {git_subdir,
        "https://github.com/whatsapp/eqwalizer.git",
        {branch, "main"},
        "eqwalizer_rebar3"}}
]}.
```

## Project Discovery

When used as a language server (via the `elp server` command), ELP attempts to automatically discover project configuration when you first open a file.

It does this by searching upward from the file directory until it finds one of

- `.elp.toml`
- `rebar.config` or `rebar.config.script`
- `build_info.json`

in that order. Processing of each is described below.

### .elp.toml

This file is used as an absolute marker for the root of an ELP project.
If one is found, it will stop further upward searching for alternative configuration.

It contains overall configuration for ELP, which is minimal at this stage.

If the configuration, described below, has enough to load the project, this is used. Otherwise the rest of discovery takes place, but restricted to this directory only.

Here is an example of a full `.elp.toml` file. all sections are optional, an empty one can be used to mark the root of a project.  The sections in it are described below.

```toml
build_info = my_hand_crafted_build_info.json

[eqwalizer]
enable_all = true

[buck]
enabled = false

[rebar]
profile = "test"
```

#### .elp.toml build_info

This gives a path to a JSON file describing the project. This is only honoured if `buck.enabled` is `false` or missing. The format is described below in [build_info.json](#build_infojson)

#### .elp.toml eqwalizer

By default we enable eqwalizer on all non-test modules. It is also disabled for modules having `@generated` anywhere in their first 2000 characters. This can be overriden per module as follows

- `-eqwalizer(ignore).` attribute opts module out unconditionally
- `-typing([eqwalizer]).` attribute opts module in unconditionally, including in test modules

Putting `enable_all = false` in the `[eqwalizer]` section disables it for
all modules by default, but still honours the module-specific overrides listed above.

#### .elp.toml buck

The `[buck]` section is used to control loading a project that is built using [buck2](https://buck2.build/). It is used internally at Meta, and has Erlang support built in. See [this presentation](https://youtu.be/4ALgsBqNBhQ) for details.

:::note

Up to and including the [2024-02-07](https://github.com/WhatsApp/erlang-language-platform/releases/tag/2024-02-07) release, the github version is not built with buck2 support enabled. This will change soon, once we tweak the tests.

:::

#### .elp.toml rebar

The `[rebar]` section is used to specify the profile to be used by rebar, if the project loads via rebar.  If left out, and a rebar project is loaded, the `test` profile will be used as default.

Note: this section is only consulted if rebar project loading takes place, described in the next section.

### Rebar

If `rebar.config` or `rebar.config.script` are found, ELP runs

`rebar3 as test help build_info`

to check that rebar3 is in the path and the `build_info` plugin is installed in the `rebar.config` file.


:::tip

If you are expecting a rebar project to be found, make sure this command succeeds, and that `rebar3` is in the path seen by the ELP server when it launches.

:::

If so, it loads a the project based on the `build_info` output.


### build_info.json

If the above checks fail, it looks for a file called `build_info.json`, which contains the same information as output by the `rebar3` `build_info` plugin, just in JSON format.

:::tip

You can generate an example to customize for your own configuration by using the following command on a configured `rebar3` project, in the directory where the `rebar.config` file exists.

```
elp build-info --project . --json --to build_info.json
```

:::

The `.json` file should be structured in this way:
```
{
  "apps": [app list],
  "deps": [app list],      // 3rd party dependencies (not type-checked), defaults to []
}
```
where an `app` is a map structured as such:
```
{
  "name": "app_name",
  "dir": "path/to/app",                         // Relative to project root
  "src_dirs": ["path/to/src", ...],             // Relative to app dir, defaults to ["src"]
  "extra_src_dirs": ["path/to/extra_src", ...], // Relative to app dir, defaults to []
  "ebin": "path/to/ebin",                       // Relative to app dir, defaults to "ebin"
  "include_dirs": ["include", ...],             // Relative to app dir, defaults to []
  "macros": ["MACRO", ...],                     // Defaults to []
}
```

### Last resort discovery

Given the discovery process is kicked off when your LSP client opens an erlang file, it starts at that location and searches upwards. If it is in a `src` directory, it assumes you are in an erlang app with the parent of `src` as its name. It is pretty basic, and does not (currently) look for the other normal directories, such as "include" or "test", nor other likely application directories.
