defmodule ElixirRanch.ListenerSup do
  use Supervisor

  def start_link(args) do
    Supervisor.start_link(__MODULE__, args)
  end

  @impl true
  def init({}) do
    children = [
      {ElixirRanch.Listeners.Echo, [{:port, 5555}]},
      {ElixirRanch.Listeners.EchoServer, [{:port, 5556}]}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
