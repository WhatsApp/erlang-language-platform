---
sidebar_position: 1
---

# Configure Your Project

When used as a language server - via the [elp server](../cli.md#elp-server) command or via a text editor extension -, ELP needs to be aware of the structure of the project. This is essential for ELP to correctly identify dependencies, header files and the alike. Failing the discovery phase results in a degraded language server, where features such as _auto-completion_ or _go-to-definition_ do not work as expected.

For `rebar3` projects (i.e. when a `rebar.config` or `rebar.config.script` file is encountered), ELP attempts to automatically discover the structure of the project when you first open a file. It is also possible to explicitly create a configuration file, named [.elp.toml](./elp-toml.md) in the root directory of a project, to provide ELP explicit information about the project structure.

ELP can also load the project structure via the [Buck2](./elp-toml.md#buck) build system or, for [custom projects](./custom-project.md), via a dedicated [build_info.json](./custom-project.md#the-build_infojson-format) file.
