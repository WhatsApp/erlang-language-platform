defmodule Mix.Tasks.Elp.BuildInfo do
  @shortdoc "Describes a mix project to ELP in a build_info.json"

  @moduledoc """
  Writes a `build_info.json` describing a mix project for ELP.

      mix elp.build_info [--project DIR] [--output FILE] [--no-elp-toml]

  The project itself becomes the application ELP checks, every dependency and
  the Elixir applications become its dependencies. Each application lists its
  source directories, its `ebin` directory and the modules ELP cannot index by
  itself: those compiled from `.ex` files, since Elixir module names cannot be
  derived from file names, each with the file it came from, and those with no
  source at all, such as the Erlang modules of the Elixir compiler, each with
  its BEAM file.

  ELP reads Elixir modules from their BEAM files, so compile the project first.
  Unless `--no-elp-toml` is given, an `.elp.toml` pointing at the output is
  written next to it when there is none.
  """

  use Mix.Task

  alias ElixirService.Beam

  @switches [project: :string, output: :string, elp_toml: :boolean]
  @elixir_apps [:elixir, :logger, :eex, :ex_unit, :iex, :mix]

  # Evaluated in the project described, see `project_facts/1`.
  @facts_marker "ELP_BUILD_INFO:"
  @facts_script """
  config = Mix.Project.config()

  facts = %{
    app: config[:app],
    elixirc_paths: Keyword.get(config, :elixirc_paths, ["lib"]),
    erlc_paths: Keyword.get(config, :erlc_paths, ["src"]),
    erlc_include_path: Keyword.get(config, :erlc_include_path, "include"),
    compile_path: Mix.Project.compile_path(),
    build_path: Mix.Project.build_path(),
    deps_paths: Mix.Project.deps_paths()
  }

  IO.puts("#{@facts_marker}" <> Base.encode64(:erlang.term_to_binary(facts)))
  """

  @impl Mix.Task
  def run(args) do
    {opts, [], []} = OptionParser.parse(args, strict: @switches)
    project_dir = Path.expand(opts[:project] || ".")
    generate(project_dir, project_facts(project_dir), opts)
  end

  # What the build_info needs to know about the project: from the mix project
  # itself when this task runs in it, otherwise from a `mix run` in its
  # directory, since a project loaded from elsewhere reports no dependencies.
  defp project_facts(dir) do
    if Mix.Project.get() && Path.expand(File.cwd!()) == dir do
      {output, _bindings} =
        ExUnit.CaptureIO.with_io(fn -> Code.eval_string(@facts_script) end)

      decode_facts(output)
    else
      args = ["run", "--no-start", "--no-compile", "--no-deps-check", "-e", @facts_script]
      env = [{"MIX_ENV", Atom.to_string(Mix.env())}]

      case System.cmd("mix", args, cd: dir, env: env, stderr_to_stdout: true) do
        {output, 0} ->
          decode_facts(output)

        {output, status} ->
          Mix.raise("mix run in #{dir} failed with status #{status}:\n#{output}")
      end
    end
  end

  defp decode_facts(output) do
    output
    |> String.split("\n")
    |> Enum.find_value(fn line ->
      case String.trim(line) do
        @facts_marker <> encoded -> :erlang.binary_to_term(Base.decode64!(encoded))
        _ -> nil
      end
    end) || Mix.raise("no project facts in mix run output:\n#{output}")
  end

  defp generate(project_dir, facts, opts) do
    output = Path.expand(opts[:output] || "build_info.json", project_dir)
    root = Path.dirname(output)

    apps = [project_app(facts, project_dir, root)]

    deps =
      Enum.map(facts.deps_paths, fn {dep, path} ->
        dep_app(dep, path, facts.build_path, root)
      end) ++ Enum.flat_map(@elixir_apps, &elixir_app(&1, root))

    File.write!(output, encode(%{"apps" => apps, "deps" => deps}))
    Mix.shell().info("Wrote #{Path.relative_to_cwd(output)}")

    if opts[:elp_toml] != false, do: write_elp_toml(root, output)
  end

  defp project_app(facts, dir, root) do
    ebin = facts.compile_path

    unless File.dir?(ebin) do
      Mix.shell().error(
        "#{ebin} does not exist: compile the project first, or ELP finds no modules"
      )
    end

    src_dirs = facts.elixirc_paths ++ facts.erlc_paths

    app_entry(
      to_string(facts.app),
      dir,
      root,
      src_dirs,
      ["test"],
      [facts.erlc_include_path],
      ebin
    )
  end

  defp dep_app(dep, path, build_path, root) do
    ebin = Path.join([build_path, "lib", to_string(dep), "ebin"])
    app_entry(to_string(dep), path, root, ["lib", "src"], [], ["include"], ebin)
  end

  defp elixir_app(app, root) do
    case :code.lib_dir(app) do
      {:error, _} ->
        []

      dir ->
        dir = Path.expand(to_string(dir))
        [app_entry(to_string(app), dir, root, ["lib", "src"], [], [], Path.join(dir, "ebin"))]
    end
  end

  defp app_entry(name, dir, root, src_dirs, extra_src_dirs, include_dirs, ebin) do
    existing = fn dirs -> Enum.filter(dirs, &File.dir?(Path.join(dir, &1))) end

    %{
      "name" => name,
      "dir" => relative(dir, root),
      "src_dirs" => existing.(src_dirs),
      "extra_src_dirs" => existing.(extra_src_dirs),
      "include_dirs" => existing.(include_dirs),
      "ebin" => relative(ebin, dir),
      "modules" => module_map(ebin, dir)
    }
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
    with {:ok, source} <- Beam.source(beam),
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
