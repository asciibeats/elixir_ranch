# A guide on how to use Ranch with Elixir
Since the top search results where outdated and generally a bit unsatisfactory to me, here is my example based on [the official guide for Erlang](https://ninenines.eu/docs/en/ranch/2.1/guide/).

## Configuration
Add [Ranch](https://ninenines.eu/docs/en/ranch/2.1/guide/) to the list of dependencies in your mix.exs:
```elixir
defp deps do
  [
    {:ranch, "~> 2.1.0"}
  ]
end
```
## [Protocols](https://ninenines.eu/docs/en/ranch/2.1/guide/protocols/)
In accordance with the official guide, I created a simple echo server, which just responds with the message one sends to it:
```elixir
defmodule ElixirRanch.Protocols.Echo do
  @behaviour :ranch_protocol
  @timeout 5000

  def start_link(ref, transport, opts) do
    {:ok, spawn_link(__MODULE__, :init, [ref, transport, opts])}
  end

  def init(ref, transport, _opts) do
    {:ok, socket} = :ranch.handshake(ref)
    loop(socket, transport)
  end

  defp loop(socket, transport) do
    case transport.recv(socket, 0, @timeout) do
      {:ok, data} ->
        :ok = transport.send(socket, data)
        loop(socket, transport)
      _ ->
        transport.close(socket)
    end
  end
end
```
Or, if you like to use [GenServer](https://hexdocs.pm/elixir/GenServer.html):
```elixir
defmodule ElixirRanch.Protocols.EchoServer do
  use GenServer

  @behaviour :ranch_protocol
  @timeout 5000

  @impl true
  def start_link(ref, transport, opts) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [{ref, transport, opts}])}
  end

  @impl true
  def init({ref, transport, _opts}) do
    {:ok, socket} = :ranch.handshake(ref)
    :ok = transport.setopts(socket, [{:active, :once}])
    :gen_server.enter_loop(__MODULE__, [], {socket, transport}, @timeout)
  end

  @impl true
  def handle_info({:tcp, socket, data}, {socket, transport} = state) do
    :ok = transport.send(socket, data)
    :ok = transport.setopts(socket, [{:active, :once}])
    {:noreply, state, @timeout}
  end

  @impl true
  def handle_info(_, {socket, transport} = state) do
    transport.close(socket)
    {:stop, :shutdown, state}
  end
end
```
## [Listeners](https://ninenines.eu/docs/en/ranch/2.1/guide/listeners/)
For clarity I encapsulated both listeners in a module:
```elixir
defmodule ElixirRanch.Listeners.Echo do
  def child_spec(opts) do
    :ranch.child_spec(__MODULE__, :ranch_tcp, opts, ElixirRanch.Protocols.Echo, [])
  end
end
```
Or for the GenServer:
```elixir
defmodule ElixirRanch.Listeners.EchoServer do
  def child_spec(opts) do
    :ranch.child_spec(__MODULE__, :ranch_tcp, opts, ElixirRanch.Protocols.EchoServer, [])
  end
end
```
Then I [embedded](https://ninenines.eu/docs/en/ranch/2.1/guide/embedded/) both listeners into a supervision tree:
```elixir
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
```
## Application
To put it all together, I added the listener supervisor to an application file...
```elixir
defmodule ElixirRanch.Application do
  use Application

  def start(_type, _args) do
    children = [
      {ElixirRanch.ListenerSup, {}}
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
```
...and added the application to my mix.exs, so it would start automatically:
```elixir
def application do
  [
    mod: {ElixirRanch.Application, []}
  ]
end
```
## Try it!
Clone this repo and get Ranch:
```
git clone https://github.com/asciibeats/elixir_ranch.git
cd elixir_ranch
mix deps.get
```
Start the server:
```
mix run --no-halt
```
Connect to it:
```
telnet localhost 5555
```
Type something, press ENTER and the server should respond with the same message. Wait five seconds for the connection to time out. Change the port to 5556 to try the protocol using GenServer. It should do the same.

I hope you found this little guide helpful!
