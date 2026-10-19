defmodule EqwalizerSupport.OverlayTest do
  @moduledoc """
  The overrides for the Elixir standard library, `:eqwalizer_specs` and
  `:eqwalizer_types`, and their separation from the OTP ones in `src/`.
  """
  use ExUnit.Case, async: true

  @src Path.expand("../src", __DIR__)

  test "every Elixir spec overrides an 'Elixir.Module:function' and has a stub clause" do
    {:ok, specs} = Code.Typespec.fetch_specs(:eqwalizer_specs)
    overrides = for {{name, arity}, _} <- specs, name != :__info__, do: {name, arity}
    assert overrides != []

    assert Enum.sort(overrides) ==
             Enum.sort(
               :eqwalizer_specs.module_info(:exports) --
                 [__info__: 1, module_info: 0, module_info: 1]
             )

    for {name, _arity} <- overrides, do: assert(Atom.to_string(name) =~ ~r/^Elixir\.[^:]+:[^:]+$/)
  end

  test "every Elixir type override is exported and named 'Elixir.Module:type'" do
    {:ok, types} = Code.Typespec.fetch_types(:eqwalizer_types)
    assert types != []

    for {kind, {name, _body, _args}} <- types do
      assert kind == :type
      assert Atom.to_string(name) =~ ~r/^Elixir\.[^:]+:[^:]+$/
    end
  end

  test "the OTP overrides in src stay free of Elixir entries" do
    specs = for {:attribute, _, :spec, {{name, _}, _}} <- parse("eqwalizer_specs.erl"), do: name
    types = for {:attribute, _, :type, {name, _, _}} <- parse("eqwalizer_types.erl"), do: name
    assert specs != [] and types != []
    refute Enum.any?(specs ++ types, &String.starts_with?(Atom.to_string(&1), "Elixir."))
  end

  defp parse(file) do
    {:ok, forms} = :epp.parse_file(String.to_charlist(Path.join(@src, file)), [])
    assert [] == for({:error, error} <- forms, do: error)
    forms
  end
end
