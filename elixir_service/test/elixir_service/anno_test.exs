defmodule ElixirService.AnnoTest do
  use ExUnit.Case, async: true

  alias ElixirService.Anno
  alias ElixirService.Beam

  setup_all do
    fixture = Application.fetch_env!(:elixir_service, :fixture)
    ebin = Application.fetch_env!(:elixir_service, :fixture_ebin)
    text = File.read!(fixture)
    {:ok, beam} = Beam.find(ebin, fixture, Hello)
    {:ok, forms} = Beam.forms(beam)
    %{text: text, forms: Anno.to_byte_ranges(forms, text)}
  end

  test "a function starts at its name, as a one-byte range", %{text: text, forms: forms} do
    {:function, {start, stop}, :add, 2, _clauses} = find_function(forms, :add, 2)
    assert start == offset_of(text, "add(a, b)")
    assert stop == start + 1
  end

  test "a column after multibyte characters is a byte offset", %{text: text, forms: forms} do
    {:function, _, :uni, 1, [{:clause, _, _, _, [{:tuple, _, [_string, {:var, {start, _}, _}]}]}]} =
      find_function(forms, :uni, 1)

    assert start == offset_of(text, "a}")
  end

  test "generated code maps to the start of the file", %{forms: forms} do
    assert {:function, {0, 1}, :__info__, 1, _clauses} = find_function(forms, :__info__, 1)
  end

  test "annotations inside specs are translated too", %{text: text, forms: forms} do
    {:attribute, _, :spec, {{:greet, 1}, [{:type, {start, _}, :fun, _}]}} =
      Enum.find(forms, &match?({:attribute, _, :spec, {{:greet, 1}, _}}, &1))

    assert start == offset_of(text, "greet(String.t())")
  end

  test "every annotation is a byte range within the text", %{text: text, forms: forms} do
    size = byte_size(text)

    for form <- forms do
      :erl_parse.map_anno(
        fn anno ->
          assert {start, stop} = anno
          assert 0 <= start and start <= stop and stop <= size
          anno
        end,
        form
      )
    end
  end

  defp find_function(forms, name, arity) do
    Enum.find(forms, &match?({:function, _, ^name, ^arity, _}, &1))
  end

  defp offset_of(text, needle) do
    {offset, _length} = :binary.match(text, needle)
    offset
  end
end
