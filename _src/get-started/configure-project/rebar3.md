---
sidebar_position: 2
---

# rebar3

ELP can auto-discover projects which contain a `rebar.config` or `rebar.config.script`. This requires rebar3 `3.24.0` or greater.

## Eqwalizer Support

By default, ELP integrates with the eqWAlizer type checker. ELP bundles the `eqwalizer_support` library, which provides eqWAlizer-friendly specs for common OTP functions, and adds it to your project automatically.

Specs that refer to `eqwalizer` types compile and run without the library, but other tools, such as Dialyzer, report those types as unknown. If you use such tools, add the library to your project dependencies, and ELP uses your copy:

```
{deps, [
  {eqwalizer_support,
    {git_subdir,
        "https://github.com/whatsapp/erlang-language-platform.git",
        {branch, "main"},
        "eqwalizer/eqwalizer_support"}}
]}.
```

Your project can also define its own copies of the library's modules, in any app:

- an `eqwalizer` module replaces the bundled one;
- the specs and types of `eqwalizer_specs` and `eqwalizer_types` modules are layered over the bundled ones, entry by entry: declare only the specs and types you add or override.

If you, instead, prefer to disable eqWAlizer support altogether (you will lose features such as _types on hover_), you can do so via the [.elp.toml](./elp-toml.md#eqwalizer) config file.

### Troubleshooting

#### My rebar3 project is not found

Run the following command in the project root:

```
$ rebar3 as test help experimental manifest
```
