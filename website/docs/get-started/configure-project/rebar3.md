---
sidebar_position: 3
---

# rebar3

ELP can auto-discover projects which contain a `rebar.config` or `rebar.config.script`. To do so, it requires an additional rebar3 plugin, named the _build_info_ plugin.

:::info

We are in the process of [upstreaming](https://github.com/erlang/rebar3/pull/2859) the plugin to rebar3, so this additional step will hopefully not be necessary with newer versions of rebar3.

:::

## Install the `rebar3` `build-info` plugin

To enable the plugin globally, add the following lines to your `~/.config/rebar3/rebar.config` file:

```erlang
{project_plugins, [
  {eqwalizer_rebar3,
    {git_subdir,
        "https://github.com/whatsapp/eqwalizer.git",
        {branch, "main"},
        "eqwalizer_rebar3"}}
]}.
```

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
$ rebar3 as test help build_info
```

#### build-info plugin was not installed

You need the `build-info` plugin for `rebar3` to be installed. Please refer to the [installation instructions](#install-the-rebar3-build-info-plugin).
