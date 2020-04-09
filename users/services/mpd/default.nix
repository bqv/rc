{ config ? {}, lib, pkgs, ... }:

with lib; let
  cfg = config.services.mpd;
in {
  config = mkIf cfg.enable {
    services.mpd = {
      musicDirectory = config.xdg.userDirs.music;
    };
  };
}
