defmodule ElixirService do
  @moduledoc """
  An `erlang_service` that also serves Elixir sources.

  The escript speaks the `erlang_service` stdio protocol unchanged. It starts
  the `erlang_service` application after registering `ElixirService.Lint` as
  the handler for `.ex` files and for `.beam` files, which stand in for the
  modules that have no source, so every other request is served exactly as
  `erlang_service` serves it.

  ELP runs it in place of the bundled `erlang_service` when the
  `ELP_ERLANG_SERVICE_PATH` environment variable names the escript.
  """

  # `erlang_service` compares the extension as a charlist, see
  # `erlang_service_lint:extension_handler/1`.
  @handlers %{
    ~c".ex" => ElixirService.Lint,
    ".ex" => ElixirService.Lint,
    ~c".beam" => ElixirService.Lint,
    ".beam" => ElixirService.Lint
  }

  @spec main([String.t()]) :: no_return()
  def main(_args) do
    :ok = :application.set_env(:erlang_service, :extension_handlers, @handlers)
    :erlang_service.main([])
  end
end
