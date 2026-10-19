defmodule Mix.Tasks.Elp.BuildInfoTest do
  @moduledoc """
  `mix elp.build_info` run in a probe project that depends on this library,
  as a project using it does.
  """
  use ExUnit.Case

  @library Path.expand("..", __DIR__)

  @tag :tmp_dir
  @tag timeout: 180_000
  test "describes a project depending on this library", %{tmp_dir: tmp} do
    mix!(["new", "probe"], tmp)
    project = Path.join(tmp, "probe")
    add_dependency(project)
    mix!(["compile"], project)
    mix!(["elp.build_info", "--no-elp-toml"], project)

    %{"apps" => [app], "deps" => deps} =
      :json.decode(File.read!(Path.join(project, "build_info.json")))

    refute File.exists?(Path.join(project, ".elp.toml"))

    assert app["name"] == "probe"
    assert app["dir"] == "."
    assert app["src_dirs"] == ["lib"]
    assert app["extra_src_dirs"] == ["test"]
    assert app["modules"] == %{"Elixir.Probe" => "lib/probe.ex"}

    # The library itself: `lib` alone, where the Elixir overrides are, so that
    # ELP layers them over its bundled copy of the Erlang ones in `src`.
    support = Enum.find(deps, &(&1["name"] == "eqwalizer_support"))
    assert support["src_dirs"] == ["lib"]
    refute Map.has_key?(support, "macros")
    support_dir = Path.expand(support["dir"], project)
    assert File.regular?(Path.join([support_dir, "lib", "eqwalizer_specs.erl"]))
    refute Map.has_key?(support["modules"], "eqwalizer_specs")
    assert Map.has_key?(support["modules"], "Elixir.Mix.Tasks.Elp.BuildInfo")

    # The Elixir application, whose ebin holds the debug-info backend the
    # service reads BEAM files with.
    elixir = Enum.find(deps, &(&1["name"] == "elixir"))
    assert File.dir?(Path.join(Path.expand(elixir["dir"], project), elixir["ebin"]))
    assert Map.has_key?(elixir["modules"], "elixir_erl")
  end

  @tag :tmp_dir
  @tag timeout: 180_000
  test "writes an .elp.toml pointing at the output unless told not to", %{tmp_dir: tmp} do
    mix!(["new", "probe"], tmp)
    project = Path.join(tmp, "probe")
    add_dependency(project)
    mix!(["compile"], project)
    mix!(["elp.build_info", "--output", "elp/build_info.json"], project)

    assert File.read!(Path.join(project, "elp/.elp.toml")) ==
             "[build_info]\nfile = \"build_info.json\"\n"
  end

  defp add_dependency(project) do
    mix_exs = Path.join(project, "mix.exs")
    original = File.read!(mix_exs)
    dependency = "{:eqwalizer_support, path: #{inspect(@library)}, only: :dev, runtime: false}"

    updated =
      String.replace(original, "defp deps do\n    [", "defp deps do\n    [\n      #{dependency},",
        global: false
      )

    assert updated != original, "mix new's deps/0 should be where the dependency goes"
    File.write!(mix_exs, updated)
  end

  defp mix!(args, dir) do
    case System.cmd("mix", args, cd: dir, env: [{"MIX_ENV", "dev"}], stderr_to_stdout: true) do
      {_output, 0} ->
        :ok

      {output, status} ->
        flunk("mix #{Enum.join(args, " ")} in #{dir} failed with #{status}:\n#{output}")
    end
  end
end
