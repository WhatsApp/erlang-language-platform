---
sidebar_position: 5
---

# Elixir Projects

eqWAlizer can type-check Elixir code. The Elixir compiler records the Erlang
abstract forms of every module in its BEAM file, with `@spec` and `@type`
translated to `-spec` and `-type`, and eqWAlizer checks those forms as it
checks Erlang. Support is experimental and limited to type-checking: no other
ELP feature understands Elixir sources.

ELP reads Elixir modules through the `elixir_service`, an extended
`erlang_service` found under `elixir_service/` in the ELP repository, which
you build with mix:

```
cd elixir_service
mix deps.get
MIX_ENV=prod mix escript.build
```

Compile your mix project, then describe it to ELP from the `elixir_service`
directory:

```
mix elp.build_info --project /path/to/project
```

This writes a `build_info.json` into the project, in the
[custom project format](./custom-project.md), and an `.elp.toml` pointing at
it when there is none. The project is the application ELP checks. Its
dependencies and the Elixir applications are listed as dependencies, each
with its `ebin` directory and a `modules` map naming the modules ELP cannot
index by itself: those compiled from `.ex` files, with the file each came
from, and those with no source at all, with their BEAM file.

Then run ELP with the `elixir_service` escript in place of its bundled one:

```
cd /path/to/project
ELP_ERLANG_SERVICE_PATH=/path/to/elixir_service/_build/elixir_service elp eqwalize-all
```

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

The Elixir standard library is seen through its specs. Many of them, such as
those of `Enum`, return `term()`, so idiomatic pipelines are checked loosely
and sometimes reported where the Erlang equivalent, with type variables,
would not be.
