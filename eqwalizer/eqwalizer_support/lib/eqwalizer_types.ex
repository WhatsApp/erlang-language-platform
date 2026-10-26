defmodule :eqwalizer_types do
  @moduledoc """
  Type overrides for the Elixir standard library, for better type-checking
  with eqWAlizer. See `:eqwalizer_specs`, next to this module, for how ELP
  reads it and layers it over its bundled copy of the OTP overrides.

  Elixir declares the elements of enumerables, the values of maps and keyword
  lists, and accumulators as `any()`, which eqWAlizer reads as `term()`: a
  value that fits nowhere specific. Elixir code relies on these being
  whatever was put in, so they are `dynamic()` here.
  """

  @type unquote(:"Elixir.Access:key")() :: dynamic
  @type unquote(:"Elixir.Access:value")() :: dynamic

  @type unquote(:"Elixir.Application:value")() :: dynamic

  @type unquote(:"Elixir.Collectable:t")() :: dynamic

  @type unquote(:"Elixir.Enum:acc")() :: dynamic
  @type unquote(:"Elixir.Enum:default")() :: dynamic
  @type unquote(:"Elixir.Enum:element")() :: dynamic

  # Elixir lists each option as its own tuple, which a keyword list built at
  # runtime never matches.
  @type unquote(:"Elixir.GenServer:options")() :: Keyword.t()

  @type unquote(:"Elixir.Keyword:default")() :: dynamic
  @type unquote(:"Elixir.Keyword:value")() :: dynamic

  @type unquote(:"Elixir.Map:key")() :: dynamic
  @type unquote(:"Elixir.Map:value")() :: dynamic

  @type unquote(:"Elixir.MapSet:value")() :: dynamic

  @type unquote(:"Elixir.Stream:acc")() :: dynamic
  @type unquote(:"Elixir.Stream:default")() :: dynamic
  @type unquote(:"Elixir.Stream:element")() :: dynamic
end
