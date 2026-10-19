defmodule EqwalizerSupport.OverlayTest do
  @moduledoc """
  The overrides for the Elixir standard library in `lib/`, and their
  separation from the Erlang ones in `src/`.
  """
  use ExUnit.Case, async: true

  @lib Path.expand("../lib", __DIR__)
  @src Path.expand("../src", __DIR__)

  test "every Elixir spec overrides an 'Elixir.Module:function' and has a stub clause" do
    forms = parse(@lib, "eqwalizer_specs.erl")
    specs = for {:attribute, _, :spec, {{name, arity}, _}} <- forms, do: {name, arity}
    funs = for {:function, _, name, arity, _} <- forms, do: {name, arity}
    assert specs != []
    assert Enum.sort(specs) == Enum.sort(funs)
    for {name, _arity} <- specs, do: assert(Atom.to_string(name) =~ ~r/^Elixir\.[^:]+:[^:]+$/)
  end

  test "every Elixir type override is exported and named 'Elixir.Module:type'" do
    forms = parse(@lib, "eqwalizer_types.erl")
    types = for {:attribute, _, :type, {name, _body, args}} <- forms, do: {name, length(args)}
    exported = for {:attribute, _, :export_type, ids} <- forms, id <- ids, do: id
    assert types != []
    assert Enum.sort(types) == Enum.sort(exported)
    for {name, _arity} <- types, do: assert(Atom.to_string(name) =~ ~r/^Elixir\.[^:]+:[^:]+$/)
  end

  test "the Erlang overrides in src stay free of Elixir entries" do
    specs =
      for {:attribute, _, :spec, {{name, _}, _}} <- parse(@src, "eqwalizer_specs.erl"), do: name

    types =
      for {:attribute, _, :type, {name, _, _}} <- parse(@src, "eqwalizer_types.erl"), do: name

    assert specs != [] and types != []
    refute Enum.any?(specs ++ types, &String.starts_with?(Atom.to_string(&1), "Elixir."))
  end

  defp parse(dir, file) do
    {:ok, forms} = :epp.parse_file(String.to_charlist(Path.join(dir, file)), [])
    assert [] == for({:error, error} <- forms, do: error)
    forms
  end
end
