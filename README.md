# Beam Olympics running on a Raspberry Pi 3

Getting Beam Olympics to run on a Raspberry Pi 3 through Nerves. Phoenix leaderboard included.

## Tutorial

1. Get yourself a Raspberry Pi 3
2. Get yourself a WiFi router ([this](http://www.tp-link.com/us/products/details/TL-WR940N.html) works fine)
3. Set the `BO_WLAN_SSID` and `BO_WLAN_PSK` environment variables to the correct values
4. Go into the `beam_olympics_nerves` folder and:
  - Install nerves following [this tutorial](https://hexdocs.pm/nerves/getting-started.html)
  - Get yourself a microSD card
  - Run `mix deps.get && mix firmware && mix firmware.burn`
  - Configure the router to assign the `192.168.0.100` address to the raspberry
  - Insert SD into raspberry and boot
5. Go into the `beam_olympics_leaderboard` folder and:
  - Connect to the same WiFi router
  - Run `mix deps.get && iex --name "leaderboard@$(ipconfig getifaddr en0)" --cookie beam -S mix phoenix.server`
  - Go to [locahost:4000](http://localhost:4000)

## Preview

![preview.png](images/preview.png)

![tutorial.png](images/tutorial.png)

## Uploading HTML and RTF instructions to S3

1. Go into the `beam_olympics_leaderboard` folder
2. Copy `.envrc.example` to `.envrc`, set the env variables and source it
3. Install `pandoc`
4. Run `mix s3_upload`
