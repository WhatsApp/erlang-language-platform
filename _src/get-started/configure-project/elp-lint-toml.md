---
sidebar_position: 6
---

# The `.elp_lint.toml` Configuration File

A `.elp_lint.toml` configuration file can be added to a project's root directory to
customize the list of diagnostics (aka linters) enabled.

## A Sample `.elp_lint.toml` Configuration File

```toml
enabled_lints = [
    ]
disabled_lints = [
    'W0011', # Accessing different app's application env
    'W0014' # Cross node eval
]
```

Where you can use:

* `enabled_lints`: To enable a diagnostic that would otherwise be disabled by default
* `disabled_lints`: To disable a diagnostic that would otherwise be enabled by default

Please refer to the [Erlang Error Index](../../erlang-error-index/erlang-error-index.md) for a reference of supported diagnostic codes.
