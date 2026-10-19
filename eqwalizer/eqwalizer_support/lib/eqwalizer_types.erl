%% Type overrides for the Elixir standard library, for better type-checking
%% with eqWAlizer. See eqwalizer_specs.erl, next to this file, for how ELP
%% alone reads it and layers it over the bundled Erlang overrides.
-module(eqwalizer_types).

-export_type([
    'Elixir.Access:key'/0,
    'Elixir.Access:value'/0,
    'Elixir.Application:value'/0,
    'Elixir.Collectable:t'/0,
    'Elixir.Enum:acc'/0,
    'Elixir.Enum:default'/0,
    'Elixir.Enum:element'/0,
    'Elixir.GenServer:options'/0,
    'Elixir.Keyword:default'/0,
    'Elixir.Keyword:value'/0,
    'Elixir.Map:key'/0,
    'Elixir.Map:value'/0,
    'Elixir.MapSet:value'/0,
    'Elixir.Stream:acc'/0,
    'Elixir.Stream:default'/0,
    'Elixir.Stream:element'/0
]).

%% Elixir declares the elements of enumerables, the values of maps and
%% keyword lists, and accumulators as `any()`, which eqWAlizer reads as
%% `term()`: a value that fits nowhere specific. Elixir code relies on these
%% being whatever was put in, so they are `dynamic()` here.

-type 'Elixir.Access:key'() :: dynamic().
-type 'Elixir.Access:value'() :: dynamic().

-type 'Elixir.Application:value'() :: dynamic().

-type 'Elixir.Collectable:t'() :: dynamic().

-type 'Elixir.Enum:acc'() :: dynamic().
-type 'Elixir.Enum:default'() :: dynamic().
-type 'Elixir.Enum:element'() :: dynamic().

%% Elixir lists each option as its own tuple, which a keyword list built at
%% runtime never matches.
-type 'Elixir.GenServer:options'() :: 'Elixir.Keyword':t().

-type 'Elixir.Keyword:default'() :: dynamic().
-type 'Elixir.Keyword:value'() :: dynamic().

-type 'Elixir.Map:key'() :: dynamic().
-type 'Elixir.Map:value'() :: dynamic().

-type 'Elixir.MapSet:value'() :: dynamic().

-type 'Elixir.Stream:acc'() :: dynamic().
-type 'Elixir.Stream:default'() :: dynamic().
-type 'Elixir.Stream:element'() :: dynamic().
