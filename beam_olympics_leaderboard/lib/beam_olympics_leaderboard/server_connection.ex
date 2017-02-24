defmodule BeamOlympicsLeaderboard.ServerConnection do
  use GenServer
  require Logger

  @retry_interval 5_000

  def start_link do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    send(self(), :schedule_connection)
    {:ok, false}
  end

  def handle_info(:schedule_connection, state) do
    Process.send_after(self(), :schedule_connection, @retry_interval)
    do_connect()
    {:noreply, state}
  end

  defp do_connect do
    server = get_server()
    case Node.connect(server) do
      true ->
        Logger.info("Connected to #{server}")
      other ->
        Logger.info("Could not connect to #{server}. Return value was #{inspect other}")
    end
  end

  def get_server do
    Application.get_env(:beam_olympics_leaderboard, :server)
  end
end
