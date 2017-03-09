defmodule Mix.Tasks.S3Upload do
  use Mix.Task

  @html_filename "instructions.html"
  @rtf_filename "instructions.rtf"

  @shortdoc "Upload html and rtf version of tutorial to AWS S3. Requires pandoc"
  def run(_) do
    {:ok, _started} = Application.ensure_all_started(:hackney)

    html = build_html()
    rtf = build_rtf(html)

    upload_to_s3(@html_filename, html)
    upload_to_s3(@rtf_filename, rtf)
  end

  def build_html() do
    BeamOlympicsLeaderboard.PageView
    |> Phoenix.View.render_to_string("tutorial.html", server: get_server())
  end

  def build_rtf(html) do
    File.write @html_filename, html
    {output, _} = System.cmd "pandoc", [@html_filename , "-s", "--from=html" , "--to=rtf"]
    File.rm @html_filename
    output
  end

  defp upload_to_s3(filename, content) do
    {:ok, _} =
      ExAws.S3.put_object(get_s3_bucket(), filename, content)
      |> ExAws.request()

    IO.puts "#{filename} successfully uploaded!"
  end

  defp get_server() do
    Application.get_env(:beam_olympics_leaderboard, :server)
  end

  defp get_s3_bucket() do
    Application.get_env(:beam_olympics_leaderboard, :s3_bucket)
  end
end
