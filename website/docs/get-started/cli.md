---
sidebar_position: 3
---

# The ELP CLI

ELP can be used from the command line. Please ensure you read the [install](install.md) section to learn how to install it.

## Verify `elp` is correctly installed

:::tip

On Mac you may get a warning, saying "elp cannot be opened because the developer cannot be verified". To solve this, go to `Preferences -> Security and Privacy -> General` and add an exception for `elp`. Alternatively, you can build the project from source (see below)

:::

Open a terminal and run:

```
$ elp version
```

You should see something like:

```
elp 1.1.0+build-2024-01-18
```

If that's the case, you're ready to roll!

## Getting Help

All `elp` commands are available through the help:

```
$ elp -h

Usage: [--log-file LOG_FILE] [--no-log-buffering] [COMMAND ...]

Available options:
        --log-file <LOG_FILE>
        --no-log-buffering
    -h, --help                 Prints help information

Available commands:
    eqwalize              Eqwalize specified module
    eqwalize-all          Eqwalize all opted-in modules in a project
    eqwalize-app          Eqwalize all opted-in modules in specified application
    eqwalize-target       Eqwalize all opted-in modules in specified buck target
    lint                  Parse files in project and emit diagnostics, optionally apply fixes.
    server                Run lsp server
    generate-completions  Generate shell completions
    parse-all             Dump ast for all files in a project for specified rebar.config file
    parse-elp             Tree-sitter parse all files in a project for specified rebar.config file
    build-info            Generate build info file
    version               Print version
    shell                 Starts an interactive ELP shell
    eqwalize-stats        Return statistics about code quality for eqWAlizer
    explain               Explain a diagnostic code
    project-info          Generate project info file
    glean                 Glean indexer
```

## `elp server`

Start a LSP server. The command does not return.

```
$ elp server
```

## `elp eqwalize`

Run the _eqWAlizer_ typechecker against an Erlang module.

```
$ elp eqwalize <module>
```

## `elp eqwalize-all`

Run the _eqWAlizer_ typechecker against all _src_ modules in a project.

```
$ elp eqwalize-all
```

## `elp shell`

Run the _eqWAlizer_ typechecker in a REPL. This means the startup processing is done
once, so it's faster to run the typechecker on changed modules.

:::warning

`elp shell` relies on [watchman](https://facebook.github.io/watchman/). If you don't have it installed, it will not work.

:::

```
$ elp shell
```
