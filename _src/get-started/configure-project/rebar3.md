---
sidebar_position: 2
---

# rebar3

ELP can auto-discover projects which contain a `rebar.config` or `rebar.config.script`. This requires rebar3 `3.24.0` or greater.

## Eqwalizer Support

By default, ELP integrates with the eqWAlizer type checker. For this to work, you need to add the following to your project dependencies:

```
{deps, [
  {eqwalizer_support,
    {git_subdir,
        "https://github.com/whatsapp/eqwalizer.git",
        {branch, "main"},
        "eqwalizer_support"}}
]}.
```

If you, instead, prefer to disable eqWAlizer support altogether (you will lose features such as _types on hover_), you can do so via the [.elp.toml](./elp-toml.md#eqwalizer) config file.

### Troubleshooting

#### My rebar3 project is not found

Run the following command in the project root:

```
$ rebar3 as test help experimental manifest
```
