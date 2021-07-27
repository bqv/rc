{ config, lib, pkgs, ... }:

with lib; let
  cfg = config.services.mpd;
in {
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      mpc_cli mpg123
    ];

    services.mpd = {
      musicDirectory = config.xdg.userDirs.music;
      network.listenAddress = "any";
    };
  };
}
