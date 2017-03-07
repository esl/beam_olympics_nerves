defmodule BeamOlympicsLeaderboard.PageView do
  use BeamOlympicsLeaderboard.Web, :view

  def funky_text(text) do
    String.split(text, "")
    |> Enum.with_index
    |> Enum.map(fn({character, index}) ->
      "<span class='char#{index}'>#{character}</span>"
    end)
    |> Enum.join("")
    |> Phoenix.HTML.raw
  end
end
