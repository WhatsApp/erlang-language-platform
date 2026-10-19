---
sidebar_position: 5
---

# Elixir Projects

eqWAlizer can type-check Elixir code. The Elixir compiler records the Erlang
abstract forms of every module in its BEAM file, with `@spec` and `@type`
translated to `-spec` and `-type`, and eqWAlizer checks those forms as it
checks Erlang. Support is experimental and limited to type-checking: no other
ELP feature understands Elixir sources.

Add the `eqwalizer_support` library to the project's development
dependencies:

```elixir
defp deps do
  [
    {:eqwalizer_support,
     git: "https://github.com/whatsapp/erlang-language-platform.git",
     sparse: "eqwalizer/eqwalizer_support",
     only: :dev,
     runtime: false}
  ]
end
```

Compile the project, then describe it to ELP:

```
mix deps.get
mix compile
mix elp.build_info
```

This writes a `build_info.json` into the project, in the
[custom project format](./custom-project.md), and an `.elp.toml` pointing at
it when there is none; both are build artifacts, for version control to
ignore. The project is the application ELP checks. Its dependencies and the
Elixir applications are listed as dependencies, each with its `ebin`
directory and a `modules` map naming the modules ELP cannot index by itself:
those compiled from `.ex` files, with the file each came from, and those with
no source at all, with their BEAM file.

Then run ELP in the project:

```
elp eqwalize-all
```

ELP's service reads each Elixir module from its BEAM file through the Elixir
that compiled the project, which the build info lists as a dependency.

Rerun `mix elp.build_info` after adding modules or dependencies, and
`mix compile` after editing a module: ELP checks the build output, not the
source.

## What is checked

Every function in the project's modules is checked against its `@spec`, as
in Erlang: a function without a spec is dynamically typed. Diagnostics point
at the start of the expression, since the Elixir compiler records only where
a node starts.

Functions the compiler generates, such as `__struct__/1` and `__info__/1`,
are not checked, but their specs and exports are kept, so calls to them are.

The Elixir standard library is seen through its specs, with overrides for
the ones that would otherwise produce noise. Elixir declares the elements of
enumerables, the values of maps and keyword lists, and accumulators as
`any()`, which eqWAlizer reads as `term()`, a value that fits nowhere
specific; and functions such as `Enum.reduce/3` and `update_in/2` return
their accumulator or container as `any()` or `map()`, so a struct threaded
through them loses its type. The `eqwalizer_support` dependency redefines
those types as `dynamic()` and gives those functions type variables, the way
ELP's own overrides for `lists` and `maps` do, in the `eqwalizer_specs` and
`eqwalizer_types` modules under its `lib`, which ELP layers over its bundled
copy of the Erlang overrides.
