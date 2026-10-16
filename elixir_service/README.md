# ELP elixir service

An `erlang_service` that also serves Elixir sources, so that eqWAlizer can
type-check Elixir modules through the Erlang abstract forms the Elixir
compiler records in their BEAM files.

The escript speaks the `erlang_service` stdio protocol unchanged. Requests
for `.ex` files, and for `.beam` files standing in for modules with no
source, are served by `ElixirService.Lint`; everything else by
`erlang_service`, which this project depends on.

## Build

    mix deps.get
    MIX_ENV=prod mix escript.build

The escript lands in `_build/elixir_service`.

## Use

Compile the mix project to check, then describe it to ELP:

    cd elixir_service
    mix elp.build_info --project /path/to/project

This writes `build_info.json` into the project, and an `.elp.toml` pointing
at it when there is none. The project is the application ELP checks; its
dependencies and the Elixir applications are listed as dependencies, each
with the modules ELP cannot index by itself: those compiled from `.ex` files,
with the file each came from, and those with no source at all, with their
BEAM file.

Then run ELP with this escript in place of its bundled one:

    cd /path/to/project
    ELP_ERLANG_SERVICE_PATH=/path/to/elixir_service/_build/elixir_service elp eqwalize-all

Rerun `mix elp.build_info` after adding modules or dependencies, and
`mix compile` after editing: ELP reads Elixir modules from the build output.

## What eqWAlizer sees

The Erlang forms of each module, with `@spec` and `@type` translated to
`-spec` and `-type`, and every position mapped to the byte offset where the
node starts. Functions the compiler generates, such as `__struct__/1`, are
not checked, but their specs and exports are kept so calls to them are.
Functions without a spec are dynamically typed, as in Erlang.

The standard library's specs declare elements, values and accumulators as
`any()`, which eqWAlizer reads as a `term()` that fits nowhere specific, and
return containers as `map()` or `acc()`, so a struct threaded through
`Enum.reduce/3` or `update_in/2` loses its type. `priv/eqwalizer_support`
holds `eqwalizer_types` and `eqwalizer_specs` overrides for the worst
offenders: those types become `dynamic()`, those functions get type
variables. `mix elp.build_info` copies them into the project's build
directory as the `elixir_eqwalizer_support` dependency, which ELP layers
over its bundled OTP overrides. On the `req` library the overrides cut the
diagnostics from 118 to 22, and the rest are not the standard library's.
`--no-overlay` leaves them out.

## Test

    mix test

The protocol test runs only when the escript has been built.

## Contributing

The rules the service is developed under, for people and agents alike,
are in `AGENTS.md`: the service stays general, standard-library knowledge
goes into the support library, and rewrites of Elixir forms need approval.
