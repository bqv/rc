{ config ? {}, lib, pkgs, ... }:

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
      global = import ../../../secrets/spotify.credentials.nix;
    };
  };
}
