---
sidebar_position: 4
---

# Custom Project

If your project is not using `rebar3` or `buck2` as a build system, you can still [configure ELP to load the project configuration from a JSON file](./elp-toml.md#build-info). This section describes the format of such a file.

### The `build_info.json` format

:::tip

You can generate an example to customize for your own configuration by using the following command on a configured `rebar3` project, in the directory where the `rebar.config` file exists.

```
elp build-info --project . --to build_info.json
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
  "modules": {"module": "path/to/file", ...},   // See below, defaults to {}
}
```

The `modules` map lists the modules whose names cannot be derived from their file names, such as Elixir modules, each with the file defining it, relative to the app dir. ELP does not parse these files itself: it indexes the modules under the given names and asks the service it runs for their forms, which reads them from the app's `ebin`. See [Elixir projects](./elixir.md).
