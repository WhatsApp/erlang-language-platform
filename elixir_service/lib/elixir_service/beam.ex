defmodule ElixirService.Beam do
  @moduledoc """
  Locates the BEAM files mix compiled from a source file and reads their
  Erlang abstract forms.
  """

  @doc """
  The BEAM file in `ebin` for `module`, or, with no module, the one compiled
  from `source`. When several modules were compiled from `source`, the one
  with the shortest name wins: the outermost `defmodule`.
  """
  @spec find(Path.t(), Path.t(), module() | nil) :: {:ok, Path.t()} | {:error, term()}
  def find(ebin, _source, module) when is_atom(module) and module != nil do
    path = Path.join(ebin, "#{module}.beam")
    if File.regular?(path), do: {:ok, path}, else: {:error, {:beam_not_found, path}}
  end

  def find(ebin, source, nil) do
    case lookup(source_index(ebin), source) do
      [] -> {:error, {:no_beam_for_source, ebin}}
      beams -> {:ok, Enum.min_by(beams, &byte_size(Path.basename(&1)))}
    end
  end

  @doc "The Erlang abstract forms of a BEAM file."
  @spec forms(Path.t()) :: {:ok, [tuple()]} | {:error, term()}
  def forms(beam) do
    case :beam_lib.chunks(String.to_charlist(beam), [:abstract_code]) do
      {:ok, {_module, [abstract_code: {:raw_abstract_v1, forms}]}} -> {:ok, forms}
      {:ok, {_module, [abstract_code: :no_abstract_code]}} -> {:error, {:no_debug_info, beam}}
      {:error, :beam_lib, reason} -> {:error, {:beam_lib, reason}}
    end
  end

  @doc "The source file a BEAM file was compiled from, as recorded at compile time."
  @spec source(Path.t()) :: {:ok, Path.t()} | :error
  def source(beam) do
    with {:ok, {_module, [compile_info: info]}} <-
           :beam_lib.chunks(String.to_charlist(beam), [:compile_info]),
         source when source != nil <- Keyword.get(info, :source) do
      {:ok, IO.chardata_to_string(source)}
    else
      _ -> :error
    end
  end

  @doc """
  Maps each source file to the BEAM files in `ebin` compiled from it, cached
  per directory until the directory's modification time changes.
  """
  @spec source_index(Path.t()) :: %{Path.t() => [Path.t()]}
  def source_index(ebin) do
    key = {__MODULE__, ebin}

    mtime =
      case File.stat(ebin, time: :posix) do
        {:ok, stat} -> stat.mtime
        {:error, _} -> nil
      end

    case :persistent_term.get(key, nil) do
      {^mtime, index} ->
        index

      _ ->
        index = build_source_index(ebin)
        :persistent_term.put(key, {mtime, index})
        index
    end
  end

  defp build_source_index(ebin) do
    ebin
    |> Path.join("*.beam")
    |> Path.wildcard()
    |> Enum.reduce(%{}, fn beam, acc ->
      case source(beam) do
        {:ok, source} -> Map.update(acc, source, [beam], &[beam | &1])
        :error -> acc
      end
    end)
  end

  # The BEAM files for `source`: by its exact path, or else by the recorded
  # path sharing the longest trailing part with it, since the path recorded at
  # compile time may differ from the path ELP sees (symlinks, moved trees).
  defp lookup(index, source) do
    case Map.fetch(index, source) do
      {:ok, beams} ->
        beams

      :error ->
        index
        |> Enum.map(fn {recorded, beams} -> {common_suffix_length(recorded, source), beams} end)
        |> Enum.filter(fn {shared, _beams} -> shared > 0 end)
        |> Enum.max_by(fn {shared, _beams} -> shared end, fn -> {0, []} end)
        |> elem(1)
    end
  end

  # The number of trailing path segments two paths share.
  defp common_suffix_length(a, b) do
    a_segments = a |> Path.split() |> Enum.reverse()
    b_segments = b |> Path.split() |> Enum.reverse()

    a_segments
    |> Enum.zip(b_segments)
    |> Enum.take_while(fn {x, y} -> x == y end)
    |> length()
  end
end
