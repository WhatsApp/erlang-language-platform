defmodule ElixirService.Lint do
  @moduledoc """
  Serves `erlang_service` parse requests for `.ex` files, and for `.beam`
  files standing in for modules that have no source.

  Elixir modules are not compiled here. The forms come from the BEAM files mix
  produced, read through `:beam_lib`, which rebuilds the Erlang abstract forms
  from the Elixir debug info. The request options name the application's
  `ebin` directory and, since one `.ex` file can define several modules,
  optionally the module wanted. A `.beam` file is read as it is, and its
  forms get no positions, since there is no source to point into.

  `run/2` has the contract of `erlang_service_lint:run/2`.
  """

  alias ElixirService.Anno
  alias ElixirService.Beam

  @type segment :: {binary(), iodata()}

  @spec run(integer(), list()) :: {:ok, [segment()]} | {:error, binary()}
  def run(_id, [file_name, _file_id, options, file_text, post_process, _deterministic]) do
    file_name = IO.chardata_to_string(file_name)

    with {:ok, beam, text} <- locate(file_name, options, file_text),
         {:ok, forms} <- Beam.forms(beam) do
      forms =
        forms
        |> reject_generated_functions()
        |> Anno.to_byte_ranges(text)
        |> insert_metadata(options)

      {:ok, [{"AST", post_process.(forms, file_name)}]}
    else
      {:error, reason} -> {:error, "elixir_service: #{file_name}: #{describe(reason)}"}
    end
  end

  # The BEAM file to read and the source text its positions refer to.
  defp locate(file_name, options, file_text) do
    if Path.extname(file_name) == ".beam" do
      {:ok, file_name, ""}
    else
      with {:ok, ebin} <- fetch_ebin(options),
           {:ok, beam} <- Beam.find(ebin, file_name, module_option(options)) do
        {:ok, beam, file_text}
      end
    end
  end

  # The functions the compiler generates are not the user's code, and the
  # problems eqWAlizer finds in their bodies, which rely on dynamic typing,
  # have nowhere to be reported. Those are the functions at line 0, such as
  # `__info__/1`, and the ones with names Elixir reserves for the compiler,
  # such as `__struct__/1` and `__impl__/1`, whose bodies alone are
  # generated. Their specs and exports stay, so calls to them are checked.
  defp reject_generated_functions(forms) do
    Enum.reject(forms, fn
      {:function, anno, name, _arity, _clauses} ->
        :erl_anno.location(anno) == 0 or reserved_name?(name)

      _form ->
        false
    end)
  end

  defp reserved_name?(name) do
    name = Atom.to_string(name)
    String.starts_with?(name, "__") and String.ends_with?(name, "__")
  end

  defp fetch_ebin(options) do
    case List.keyfind(options, :ebin, 0) do
      {:ebin, ebin} -> {:ok, IO.chardata_to_string(ebin)}
      nil -> {:error, :no_ebin}
    end
  end

  defp module_option(options) do
    case List.keyfind(options, :module, 0) do
      {:module, module} when is_atom(module) -> module
      _ -> nil
    end
  end

  defp insert_metadata(forms, options) do
    case List.keyfind(options, :elp_metadata, 0) do
      {:elp_metadata, metadata} -> :elp_metadata.insert_metadata(metadata, forms)
      nil -> forms
    end
  end

  defp describe(:no_ebin),
    do:
      "no ebin directory given for the file's application (is build_info.json from `mix elp.build_info`?)"

  defp describe({:beam_not_found, path}),
    do: "BEAM file not found: #{path} (is the project compiled?)"

  defp describe({:no_beam_for_source, ebin}),
    do: "no BEAM file in #{ebin} was compiled from this file (is the project compiled?)"

  defp describe({:no_debug_info, beam}), do: "#{beam} has no debug info"
  defp describe({:beam_lib, reason}), do: "beam_lib: #{inspect(reason)}"
end
