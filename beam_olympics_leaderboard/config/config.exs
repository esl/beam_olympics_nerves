# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# Configures the endpoint
config :beam_olympics_leaderboard, BeamOlympicsLeaderboard.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "wgwXyQcYU0/K2+94w/j4FjcPDsG1M/TU0JgOnYUW+c+a9w+haYC3YQWa6JDzprPn",
  render_errors: [view: BeamOlympicsLeaderboard.ErrorView, accepts: ~w(html json)],
  pubsub: [name: BeamOlympicsLeaderboard.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"

config :beam_olympics_leaderboard,
  server: :"sfcodebeamchallenge2018@10.0.252.49",
  s3_bucket: System.get_env("AWS_S3_BUCKET")

config :ex_aws,
  access_key_id: [System.get_env("AWS_ACCESS_KEY_ID"), :instance_role],
  secret_access_key: [System.get_env("AWS_SECRET_ACCESS_KEY"), :instance_role],
  region: [System.get_env("AWS_REGION"), :instance_role]
