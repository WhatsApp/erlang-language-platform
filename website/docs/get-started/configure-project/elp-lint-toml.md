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
| include_headers    | Boolean          | If `true`, run the linter on header files as well. Some linters skip headers by default, because linting a header in isolation is not meaningful for them. |
| experimental       | Boolean          | Override the linter's experimental flag. If `true`, the linter only runs when the `--experimental` flag is passed; if `false`, it runs unconditionally even if it is experimental by default. |
| exclude_apps       | Array of Strings | List of application name [glob patterns](https://docs.rs/glob/latest/glob/struct.Pattern.html) for which this linter should be skipped. A pattern with no metacharacter matches that application name exactly. |
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

Entries are glob patterns, so a wildcard can cover a family of applications.
This matters when the build system splits one source tree across several
applications: Buck gives every Common Test suite its own application named
after the suite module, so excluding `my_app` alone leaves the suites under
`my_app/test/` reported. A trailing `*` covers both:

```toml
[linters.no_persistent_term]
exclude_apps = ["my_app*"]
```

:::caution

`*`, `?`, `[` and `]` are glob metacharacters, so an application name that
contains one is no longer compared as plain text. The effect depends on the
character:

- `*` and `?` widen the match. `my_app?` still matches an application literally
  called `my_app?`, but also matches `my_appX` — so the linter is skipped for
  more applications than before, not fewer.
- A balanced `[...]` becomes a character class and stops matching the literal
  name: `my_[ab]` matches `my_a` and `my_b`, but no longer `my_[ab]`.
- An unbalanced `[` is not a valid pattern at all, and is reported as an error
  when the configuration is loaded.

Application names do not normally contain these characters. If one does, escape
it by wrapping each metacharacter in a bracket pair — `my_[ab]` is written as
`my_[[]ab[]]`.

:::

Enable a linter only on save:

```toml
[linters.expensive_check]
enabled = true
runs_on_save_only = true
```

## [ad_hoc_lints]

Lints defined in the config file itself, rather than compiled into ELP. Each entry
under `[[ad_hoc_lints.lints]]` has a `type`; this section covers `LintMatchSsr`,
which matches [structural search](../../structural-search.md) patterns.

### A single pattern

The shortest form takes one pattern and reports under the shared
`ad-hoc:ssr-match` code:

```toml
[[ad_hoc_lints.lints]]
type = "LintMatchSsr"
ssr_pattern = "ssr: lists:reverse(lists:reverse(_@List))."
message = "Double reversal is a no-op"
severity = "warning"
```

`elp search --dump-config` emits this form.

### A named lint with several patterns

Give the lint a `name` and list its `patterns` to report every match under a
code of its own, `ad-hoc:<name>`:

```toml
[[ad_hoc_lints.lints]]
type = "LintMatchSsr"
name = "banned_config_key"
description = "This configuration key is no longer supported"
doc = "docs/lints/banned_config_key.md"
severity = "warning"
patterns = [
  { ssr = "ssr: legacy_timeout.", label = "legacy_timeout_atom" },
  { ssr = 'ssr: <<"legacy_timeout">>.', label = "legacy_timeout_binary" },
  { ssr = "ssr: retry_forever.", label = "retry_forever_atom",
    message = "Unbounded retries are not allowed" },
]
```

A name is required to make the lint individually addressable. Without one, every
SSR lint in the config shares `ad-hoc:ssr-match` and cannot be filtered,
suppressed, or configured apart from the others.

### Available keys

| Key | Type | Description |
| --- | ---- | ----------- |
| `name` | String | Makes the diagnostic code `ad-hoc:<name>`. ASCII letters, digits, `_` and `-` only; must not collide with an existing code or label. |
| `description` | String | Default message for every pattern that has no `message` of its own. |
| `doc` | String | Path to the project's documentation for this lint, reported as `docPath` in `--format json` output. A config-declared lint has no error-index page, so this is the only way a consumer can find out how to act on a match. |
| `ssr_pattern` | String | A single pattern. Mutually exclusive with `patterns`. |
| `patterns` | Array | One or more `{ ssr, label, message }` tables. Mutually exclusive with `ssr_pattern`. |
| `message` | String | Message for the single-pattern form. Use `description` with `patterns`. |
| `pattern_label` | String | Label for the single-pattern form. Use `label` per pattern with `patterns`. |
| `severity` | String | `error`, `warning`, `weak`, or `info`. Defaults to `weak`. |
| `macro_strategy` | String | `expand` (default), `no-expand`, or `visible-expand`. |
| `paren_strategy` | String | `invisible` (default) or `visible`. |

`label` surfaces as `patternLabel` in `--format json` output, alongside the
`placeholders` bound by the match — useful when one lint has several patterns and
the consumer needs to know which one fired.

### Referring to a named lint

The diagnostic code is `ad-hoc:<name>` — with no space, so that it survives the
whitespace split applied to suppression comments:

```erlang
% elp:ignore ad-hoc:banned_config_key
```

```bash
elp lint --diagnostic-filter 'ad-hoc:banned_config_key'
```

The same spelling is the key for a [`[linters.<linter>]`](#linters) section. TOML
bare keys cannot contain `:`, so it must be quoted:

```toml
[linters."ad-hoc:banned_config_key"]
severity = "error"
include_tests = false
exclude_apps = ["legacy_app"]
```

`enabled`, `severity`, `include_tests`, `include_generated`, `include_headers`,
`experimental` and `exclude_apps` all apply. Ad-hoc lints run on test modules
and header files by default, so `include_tests = false` and
`include_headers = false` are the way to keep one out of test code and headers
respectively.

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
| `FunArity` | Argument position containing a fun expression — arity is inferred from the clause parameter count (`fun(_, _) -> ok end` → arity 2) or from a fun reference (`fun M:F/A` → arity from the integer literal) |
| Any other  | Ignored (serves as documentation, e.g., `Node`, `Opts`, `_`) |

**Validation rules:**
- `Function` and `Args`/`Arity`/`FunArity` must both be present or both absent
- If neither `Function` nor `Args`/`Arity`/`FunArity` is present, at least `Module` or `[Module]` is required (module-arg-only pattern, used for rename support)
- At most one `Module` or `[Module]` argument
- At most one `Function` argument
- At most one of `Args`, `Arity`, or `FunArity` (not multiple)

There are two kinds of patterns:

1. **Dynamic call patterns** — include `Function` and `Args`/`Arity`/`FunArity`. Used by find-references to resolve dynamic calls.
2. **Module-arg-only patterns** — include only `Module` or `[Module]`, without `Function`/`Args`/`Arity`/`FunArity`. Used by rename to track module name arguments.

**Example:**

```toml
[dynamic_calls]
patterns = [
  # Dynamic call patterns (resolve calls)
  "my_rpc:call(Node, Module, Function, Args)",
  "my_utils:check_exported(Module, Function, Arity)",
  "my_mock:expect(Module, Function, FunArity)",
  "my_apply(Function, Args)",
  # Module-arg-only patterns (rename support)
  "my_mock:setup(Module, _)",
  "my_mock:setup([Module], _)",
]
```

In this example:
- `my_rpc:call/4` — ELP learns that argument 2 is the module, argument 3 is the function, and argument 4 is the args list
- `my_utils:check_exported/3` — a local utility where argument 3 is a direct arity integer
- `my_mock:expect/3` — argument 3 is a fun expression; arity is inferred from the clause parameter count (`fun(_, _) -> ok end` → arity 2) or from a fun reference (`fun M:F/A`)
- `my_apply/2` — an unqualified function with no module argument
- `my_mock:setup/2` — two patterns together tell ELP that argument 1 can be either a single module atom or a list of modules (rename will update both forms)
