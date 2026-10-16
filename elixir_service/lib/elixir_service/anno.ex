defmodule ElixirService.Anno do
  @moduledoc """
  Rewrites the annotations of Erlang abstract forms into ELP byte ranges.

  ELP's own parser annotates every node with `{StartByte, EndByte}` instead of
  a line and column, and eqWAlizer needs byte offsets to place its diagnostics.
  The Elixir compiler records only where a node starts, so each range is one
  byte long.
  """

  @type forms :: [tuple()]

  @spec to_byte_ranges(forms, binary()) :: forms
  def to_byte_ranges(forms, text) do
    index = line_index(text)
    size = byte_size(text)
    translate = fn anno -> translate(anno, text, index, size) end
    Enum.map(forms, &:erl_parse.map_anno(translate, &1))
  end

  defp translate(anno, text, index, size) do
    start = anno |> :erl_anno.location() |> offset(text, index, size)
    {start, min(start + 1, size)}
  end

  # The byte offset of a location. Line 0 marks generated code and maps to the
  # start of the file; a line past the end maps to the end of the file.
  defp offset(line, text, index, size) when is_integer(line) do
    offset({line, 1}, text, index, size)
  end

  defp offset({line, _column}, _text, _index, _size) when line < 1, do: 0

  defp offset({line, column}, text, index, size) do
    if line > tuple_size(index) do
      size
    else
      line_start = elem(index, line - 1)
      rest = binary_part(text, line_start, size - line_start)
      line_start + column_bytes(rest, column - 1)
    end
  end

  # The byte length of the first `count` characters of the line starting
  # `text`, or of the whole line when it is shorter.
  defp column_bytes(_text, count) when count <= 0, do: 0
  defp column_bytes(<<>>, _count), do: 0
  defp column_bytes(<<?\n, _::binary>>, _count), do: 0

  defp column_bytes(<<_::utf8, rest::binary>> = text, count) do
    byte_size(text) - byte_size(rest) + column_bytes(rest, count - 1)
  end

  defp column_bytes(<<_byte, rest::binary>>, count) do
    1 + column_bytes(rest, count - 1)
  end

  # The byte offset of every line start, by line number from 1.
  defp line_index(text) do
    starts = Enum.map(:binary.matches(text, "\n"), fn {pos, 1} -> pos + 1 end)
    List.to_tuple([0 | starts])
  end
end
