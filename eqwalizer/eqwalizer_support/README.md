# eqwalizer_support library

This library provides API and integrations points for eqWAlizer, including alternative (more type-checking-friendly) specs for some essential functions and definitions for some types from OTP libraries and, for Elixir projects, from the Elixir standard library, along with the mix tasks that describe a project to ELP.

ELP bundles this library and adds it to every project. Specs that refer to `eqwalizer` types compile and run without the library, but other tools, such as Dialyzer, report those types as unknown. If you use such tools, add the library to your dependencies, and ELP uses your copy.

Minimal rebar3 config:

```
{deps, [
    {eqwalizer_support, {git_subdir, "https://github.com/whatsapp/erlang-language-platform.git", {branch, "main"}, "eqwalizer/eqwalizer_support"}}
]}.
```

## Elixir projects

Add the library to the project's development dependencies:

```elixir
{:eqwalizer_support,
 git: "https://github.com/whatsapp/erlang-language-platform.git",
 sparse: "eqwalizer/eqwalizer_support",
 only: :dev,
 runtime: false}
```

Then `mix elp.build_info` describes the compiled project to ELP, `mix elp.eqwalize_all` compiles the project and runs `elp eqwalize-all` in it, and `mix elp.eqwalize Hello.World` checks the modules named. See [Elixir projects](https://whatsapp.github.io/erlang-language-platform/docs/get-started/configure-project/elixir/) in the ELP documentation for what is checked.

The overrides for the Elixir standard library are the Elixir modules `:eqwalizer_specs` and `:eqwalizer_types` under `lib`, apart from the OTP ones in `src`, which only rebar3 builds compile. Mix compiles the Elixir ones, ELP reads them from their BEAM files as it reads the project's own modules and layers them over its bundled copy of the OTP overrides; `mix elp.build_info` lists `lib` as the library's only source directory, so the copy in `src` stays out of the way.

## Development

`mix test` parses the overrides and describes a probe project that depends on this library. The rules the Elixir support is developed under are in [AGENTS.md](./AGENTS.md).
