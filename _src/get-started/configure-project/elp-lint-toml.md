---
sidebar_position: 6
---

# The `.elp_lint.toml` Configuration File

A `.elp_lint.toml` configuration file can be added to a project's root directory to
customize the list of diagnostics (aka linters) enabled and other behaviours.

## A Sample `.elp_lint.toml` Configuration File

```toml
[erlang_service]
warnings_as_errors = true

[linters.binary_string_to_sigil]
enabled = true

[linters.W0010]
enabled = false

[linters.no_garbage_collect]
severity = "error"
```

## [erlang_service]

Through the `erlang_service` section, you can configure the behaviour of the Erlang Service (the Erlang sidecar next to ELP).

### warnings_as_errors

If set to `true`, warnings producing by the Erlang Service will be treated as errors.

## [linters.\<linter\>] {#linters}

Each individual linter (a.k.a. diagnostic) can be enabled, disabled, or have
its default behaviour overridden via a dedicated `[linters.<linter>]` section.

The `<linter>` key can be either:

- the diagnostic **code** (e.g. `W0010`, `W0051`), or
- the diagnostic **label** (e.g. `binary_string_to_sigil`, `no_garbage_collect`).

The full list of available diagnostics — together with their codes and labels —
is documented in the [Erlang Error Index](../../erlang-error-index/erlang-error-index.mdx).

### Enabling and disabling a linter

Use the `enabled` key to turn a linter on or off:

```toml
# Enable a diagnostic that is disabled by default
[linters.binary_string_to_sigil]
enabled = true

# Disable a diagnostic that is enabled by default
[linters.W0010]
enabled = false
```

When `enabled = false`, the linter will not produce any diagnostics for the
project. When `enabled = true`, the linter is run even if it is disabled by
default (some diagnostics — typically more opinionated ones — are off by default
and must be opted into explicitly; see for example
[W0051](../../erlang-error-index/w/W0051.md)).

### Available keys

| Key                | Type             | Description                                                                                                       |
| ------------------ | ---------------- | ----------------------------------------------------------------------------------------------------------------- |
| enabled            | Boolean          | Enable or disable this linter. Overrides the linter's default state.                                              |
| severity           | String           | Override the reported severity. One of `error`, `warning`, `weak`, `info`.                                        |
| include_tests      | Boolean          | If `true`, run the linter on test modules as well.                                                                |
| include_generated  | Boolean          | If `true`, run the linter on generated modules as well.                                                           |
| experimental       | Boolean          | Override the linter's experimental flag. If `true`, the linter only runs when the `--experimental` flag is passed; if `false`, it runs unconditionally even if it is experimental by default. |
| exclude_apps       | Array of Strings | List of application names for which this linter should be skipped.                                                |
| runs_on_save_only  | Boolean          | If `true`, the linter only runs when a file is saved (rather than on every keystroke). Useful for expensive linters. |

### Examples

Change the reported severity for a linter:

```toml
[linters.no_garbage_collect]
severity = "error"
```

Skip a linter for a set of applications:

```toml
[linters.no_persistent_term]
exclude_apps = ["my_app", "another_app"]
```

Enable a linter only on save:

```toml
[linters.expensive_check]
enabled = true
runs_on_save_only = true
```
