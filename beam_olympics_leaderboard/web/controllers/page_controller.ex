defmodule BeamOlympicsLeaderboard.PageController do
  use BeamOlympicsLeaderboard.Web, :controller

  def index(conn, _params) do
    render conn, "index.html", users: get_users()
  end

  def tutorial(conn, _params) do
    server = get_server()
    render conn, "tutorial.html", server: server
  end

  defp get_users do
    try do
      stats = GenServer.call({:bo_server, get_server()}, :stats)

      Map.get(stats, :players)
      |> Enum.sort_by(&(&1.score), &>=/2)
    catch
      :exit, _reason -> []
    end
  end

  def get_server do
    Application.get_env(:beam_olympics_leaderboard, :server)
  end
end
