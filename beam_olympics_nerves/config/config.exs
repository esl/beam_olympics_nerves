# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# Import target specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
# Uncomment to use target specific configurations

# import_config "#{Mix.Project.config[:target]}.exs"

config :nerves, :firmware,
  rootfs_additions: "rootfs-additions"

config :beam_olympics_nerves, :wlan0,
  ssid: System.get_env("BO_WLAN_SSID"),
  psk: System.get_env("BO_WLAN_PSK"),
  key_mgmt: :"WPA-PSK"

config :mnesia,
  debug: true

config :sumo_db,
  log_queries: true,
  query_timeout: 30_000,
  wpool_opts: [overrun_warning: 100],
  storage_backends: [],
  stores: [{
    :bo_store_mnesia,
    :sumo_store_mnesia,
    [workers: 10, disc_copies: :here]
  }],
  docs: [{
    :players,
    :bo_store_mnesia,
    %{module: :bo_players}
  }],
  events: []

config :beam_olympics,
  all_tasks: [
    :bo_first_task,
    :bo_henchmen,
    :bo_awesomeness,
    :bo_missing_operations,
    :bo_permutation_sum,
    :bo_stringy,
    :bo_romans,
    :bo_tuple_counter,
    :bo_properly,
    :bo_entangled,
    :bo_fun_with_numbers,
    :bo_oldschool_flags
  ]
