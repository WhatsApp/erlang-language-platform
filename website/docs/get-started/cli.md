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
    eqwalizer-onboard     Find the modules that are ready to opt in to eqWAlizer
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

## `elp eqwalizer-onboard`

List the modules that _eqWAlizer_ does not check yet, because they are not
opted in, split into the ones that already type check and the ones with errors,
fewest errors first. Modules opted out with `-eqwalizer(ignore).` or
`ignore_modules` and generated modules are left out.

```
$ elp eqwalizer-onboard
Modules that type check (2):
  src/my_app_config.erl
  src/my_app_sup.erl
Modules with errors (2):
  src/my_app_server.erl: 4 errors
  src/my_app_parser.erl: 37 errors
```

Pass `--opt-in` to add the `-typing([eqwalizer]).` attribute to the modules
that type check. Pass `--app <APP>`, or `--target <TARGET>` in a Buck project,
to onboard one application or target at a time.

## `elp shell`

Run the _eqWAlizer_ typechecker in a REPL. This means the startup processing is done
once, so it's faster to run the typechecker on changed modules.

:::warning

`elp shell` relies on [watchman](https://facebook.github.io/watchman/). If you don't have it installed, it will not work.

:::

```
$ elp shell
```

## Environment Variables

These apply to every `elp` command, including `elp server`.

| Variable                        | Description                                                                                                                                                                                                                                                                     | Default |
| ------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------- |
| `ELP_ERLANG_SERVICE_SCHEDULERS` | Number of schedulers of the Erlang VM ELP uses to parse and lint Erlang code. Past about 16, more schedulers make that VM slower rather than faster. On hosts with fewer CPUs, only as many schedulers as there are CPUs run. An already running daemon keeps the value it started with. | 16      |
