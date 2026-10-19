defmodule Mix.Tasks.Elp.BuildInfo do
  @shortdoc "Describes the project to ELP in a build_info.json"

  @moduledoc """
  Writes a `build_info.json` describing the project to ELP, so that eqWAlizer
  can type-check it.

      mix elp.build_info [--output FILE] [--no-elp-toml]

  The project becomes the application ELP checks; every dependency and the
  Elixir applications become its dependencies. Each application lists its
  source directories, its `ebin` directory and the modules ELP cannot index by
  itself: those compiled from `.ex` files, since Elixir module names cannot be
  derived from file names, each with the file it came from, and those with no
  source at all, such as the Erlang modules of the Elixir compiler, each with
  its BEAM file.

  ELP reads Elixir modules from their BEAM files, so compile the project first.
  Unless `--no-elp-toml` is given, an `.elp.toml` pointing at the output is
  written next to it when there is none.

  This library's entry lists `lib` as its only source directory, so that the
  OTP overrides in its `src`, which ELP bundles already, stay out of the way;
  its overrides for the Elixir standard library, `:eqwalizer_specs` and
  `:eqwalizer_types`, are Elixir modules mix compiles, listed like any other.
  """

  use Mix.Task

  @switches [output: :string, elp_toml: :boolean]
  @elixir_apps [:elixir, :logger, :eex, :ex_unit, :iex, :mix]

  @impl Mix.Task
  def run(args) do
    {opts, [], []} = OptionParser.parse(args, strict: @switches)
    project_dir = Path.dirname(Mix.Project.project_file())
    output = Path.expand(opts[:output] || "build_info.json", project_dir)
    root = Path.dirname(output)

    apps = [project_app(Mix.Project.config(), project_dir, root)]

    deps =
      Enum.map(Mix.Project.deps_paths(), fn {dep, path} -> dep_app(dep, path, root) end) ++
        Enum.flat_map(@elixir_apps, &elixir_app(&1, root))

    File.mkdir_p!(root)
    File.write!(output, encode(%{"apps" => apps, "deps" => deps}))
    Mix.shell().info("Wrote #{Path.relative_to_cwd(output)}")

    if opts[:elp_toml] != false, do: write_elp_toml(root, output)
  end

  defp project_app(config, dir, root) do
    ebin = Mix.Project.compile_path()

    unless File.dir?(ebin) do
      Mix.shell().error(
        "#{ebin} does not exist: compile the project first, or ELP finds no modules"
      )
    end

    src_dirs =
      Keyword.get(config, :elixirc_paths, ["lib"]) ++ Keyword.get(config, :erlc_paths, ["src"])

    include_dirs = [Keyword.get(config, :erlc_include_path, "include")]
    app_entry(to_string(config[:app]), dir, root, src_dirs, ["test"], include_dirs, ebin)
  end

  # This library's Erlang sources in `src` duplicate ELP's bundled copy of
  # the OTP overrides; its Elixir modules carry the overrides ELP layers over
  # that copy.
  defp dep_app(:eqwalizer_support = dep, path, root) do
    app_entry(to_string(dep), path, root, ["lib"], [], [], dep_ebin(dep))
  end

  defp dep_app(dep, path, root) do
    app_entry(to_string(dep), path, root, ["lib", "src"], [], ["include"], dep_ebin(dep))
  end

  defp dep_ebin(dep), do: Path.join([Mix.Project.build_path(), "lib", to_string(dep), "ebin"])

  defp elixir_app(app, root) do
    case :code.lib_dir(app) do
      {:error, _} ->
        []

      dir ->
        dir = Path.expand(to_string(dir))

        [
          app_entry(
            to_string(app),
            dir,
            root,
            ["lib", "src"],
            [],
            [],
            Path.join(dir, "ebin")
          )
        ]
    end
  end

  # An app with no build output lists no modules.
  defp app_entry(name, dir, root, src_dirs, extra_src_dirs, include_dirs, ebin) do
    existing = fn dirs -> Enum.filter(dirs, &File.dir?(Path.join(dir, &1))) end

    entry = %{
      "name" => name,
      "dir" => relative(dir, root),
      "src_dirs" => existing.(src_dirs),
      "extra_src_dirs" => existing.(extra_src_dirs),
      "include_dirs" => existing.(include_dirs)
    }

    if File.dir?(ebin) do
      Map.merge(entry, %{"ebin" => relative(ebin, dir), "modules" => module_map(ebin, dir)})
    else
      entry
    end
  end

  # The modules ELP cannot index by itself, by the file to index each from.
  defp module_map(ebin, dir) do
    for beam <- Path.wildcard(Path.join(ebin, "*.beam")),
        {:ok, file} <- [module_file(beam, dir)],
        into: %{} do
      {Path.basename(beam, ".beam"), relative(file, dir)}
    end
  end

  # The file to index a module from: the `.ex` file it was compiled from when
  # that is under `dir`; none for an `.erl` file there, which ELP indexes by
  # itself; otherwise the BEAM file, the only artifact there is, as for the
  # Erlang modules of the Elixir compiler.
  defp module_file(beam, dir) do
    with {:ok, source} <- beam_source(beam),
         {:ok, file} <- resolve_source(source, dir) do
      case Path.extname(file) do
        ".ex" -> {:ok, file}
        ".erl" -> :error
        _ -> {:ok, beam}
      end
    else
      _ -> {:ok, beam}
    end
  end

  # The source file a BEAM file was compiled from, as recorded at compile time.
  defp beam_source(beam) do
    with {:ok, {_module, [compile_info: info]}} <-
           :beam_lib.chunks(String.to_charlist(beam), [:compile_info]),
         source when source != nil <- Keyword.get(info, :source) do
      {:ok, IO.chardata_to_string(source)}
    else
      _ -> :error
    end
  end

  # The file under `dir` that `source`, a path recorded at compile time, names:
  # the same path when it is there, or else the file under `dir` sharing the
  # longest trailing part with it, as for the Elixir applications, whose
  # sources were compiled elsewhere.
  defp resolve_source(source, dir) do
    if File.regular?(source) and String.starts_with?(source, dir <> "/") do
      {:ok, source}
    else
      segments = Path.split(source)

      Enum.find_value((length(segments) - 1)..1//-1, :error, fn n ->
        candidate = Path.join([dir | Enum.take(segments, -n)])
        if File.regular?(candidate), do: {:ok, candidate}
      end)
    end
  end

  # `path` relative to `base` when it is under it, as it is itself; a path
  # elsewhere, such as an Elixir application's, stays absolute.
  defp relative(base, base), do: "."

  defp relative(path, base) do
    if String.starts_with?(path, base <> "/"), do: Path.relative_to(path, base), else: path
  end

  defp encode(json) do
    if function_exported?(:json, :format, 1) do
      IO.iodata_to_binary(:json.format(json))
    else
      JSON.encode!(json)
    end
  end

  defp write_elp_toml(root, output) do
    path = Path.join(root, ".elp.toml")

    unless File.exists?(path) do
      File.write!(path, "[build_info]\nfile = \"#{relative(output, root)}\"\n")
      Mix.shell().info("Wrote #{Path.relative_to_cwd(path)}")
    end
  end
end
