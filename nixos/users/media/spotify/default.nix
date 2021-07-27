{ config, lib, pkgs, usr, ... }:

with lib; let
  cfg = config.services.spotifyd;
in {
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      spotify
      ncspot
      spotify-tui
    ];

    services.spotifyd.settings = {
      global = usr.secrets.spotify.credentials;
    };
  };
}
