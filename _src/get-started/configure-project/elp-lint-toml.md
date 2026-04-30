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
```

## [erlang_service]

Through the `erlang_service` section, you can configure the behaviour of the Erlang Service (the Erlang sidecar next to ELP).

### warnings_as_errors

If set to `true`, warnings producing by the Erlang Service will be treated as errors.
