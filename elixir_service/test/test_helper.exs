# The fixture is compiled once, into a temporary ebin the tests read BEAM
# files from, as ELP would read a project's compiled output.
fixture = Path.expand("fixtures/hello.ex", __DIR__)
ebin = Path.join(System.tmp_dir!(), "elixir_service_test_ebin_#{System.os_time()}")
File.mkdir_p!(ebin)
{:ok, _modules, _warnings} = Kernel.ParallelCompiler.compile_to_path([fixture], ebin)
Application.put_env(:elixir_service, :fixture, fixture)
Application.put_env(:elixir_service, :fixture_ebin, ebin)

# The protocol test needs the escript from `mix escript.build`.
escript = Path.expand("../_build/elixir_service", __DIR__)
Application.put_env(:elixir_service, :escript, escript)
exclude = if File.regular?(escript), do: [], else: [:escript]

ExUnit.start(exclude: exclude)
