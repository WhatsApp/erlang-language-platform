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

## [dynamic_calls]

Through the `dynamic_calls` section, you can teach ELP about additional dynamic call patterns in your codebase. This helps find-references resolve calls made through custom wrappers that forward to `Module:Function(Args)` at runtime, and helps rename track module name arguments in library calls.

### patterns

A list of pattern strings describing how your wrapper functions pass module, function, and arguments to a dynamic call.

**Format:** `[module:]function(Arg1, Arg2, ...)`

Each argument name has a special meaning:

| Name       | Meaning |
|------------|---------|
| `Module`   | Argument position containing a single target module name (atom) |
| `[Module]` | Argument position containing a list of target module names |
| `Function` | Argument position containing the target function name |
| `Args`     | Argument position containing a list of arguments (arity = list length) |
| `Arity`    | Argument position containing the arity as an integer |
| Any other  | Ignored (serves as documentation, e.g., `Node`, `Opts`, `_`) |

**Validation rules:**
- `Function` and `Args`/`Arity` must both be present or both absent
- If neither `Function` nor `Args`/`Arity` is present, at least `Module` or `[Module]` is required (module-arg-only pattern, used for rename support)
- At most one `Module` or `[Module]` argument
- At most one `Function` argument
- At most one of `Args` or `Arity` (not both)

There are two kinds of patterns:

1. **Dynamic call patterns** — include `Function` and `Args`/`Arity`. Used by find-references to resolve dynamic calls.
2. **Module-arg-only patterns** — include only `Module` or `[Module]`, without `Function`/`Args`. Used by rename to track module name arguments.

**Example:**

```toml
[dynamic_calls]
patterns = [
  # Dynamic call patterns (resolve calls)
  "my_rpc:call(Node, Module, Function, Args)",
  "my_utils:check_exported(Module, Function, Arity)",
  "my_apply(Function, Args)",
  # Module-arg-only patterns (rename support)
  "my_mock:setup(Module, _)",
  "my_mock:setup([Module], _)",
]
```

In this example:
- `my_rpc:call/4` — ELP learns that argument 2 is the module, argument 3 is the function, and argument 4 is the args list
- `my_utils:check_exported/3` — a local utility where argument 3 is a direct arity integer
- `my_apply/2` — an unqualified function with no module argument
- `my_mock:setup/2` — two patterns together tell ELP that argument 1 can be either a single module atom or a list of modules (rename will update both forms)
