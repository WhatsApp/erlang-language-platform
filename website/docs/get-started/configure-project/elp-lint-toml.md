---
sidebar_position: 6
---

# The `.elp_lint.toml` Configuration File

A `.elp_lint.toml` configuration file can be added to a project's root directory to
customize the list of diagnostics (aka linters) enabled and other behaviours.

## A Sample `.elp_lint.toml` Configuration File

```toml
enabled_lints = [
    ]
disabled_lints = [
    'W0011', # Accessing different app's application env
    'W0014' # Cross node eval
]
[erlang_service]
warnings_as_errors = true
```

## enabled_lints

You can use this option to enable diagnostics that would otherwise be disabled by default

Please refer to the [Erlang Error Index](../../erlang-error-index/erlang-error-index.mdx) for a reference of supported diagnostic codes.

## disabled_lints

You can use this option to disable a diagnostic that would otherwise be enabled by default

Please refer to the [Erlang Error Index](../../erlang-error-index/erlang-error-index.mdx) for a reference of supported diagnostic codes.

## [erlang_service]

Through the `erlang_service` section, you can configure the behaviour of the Erlang Service (the Erlang sidecar next to ELP).

### warnings_as_errors

If set to `true`, warnings producing by the Erlang Service will be treated as errors.
