{ config ? {}, lib, pkgs, ... }:

with lib; let
  cfg = config.programs.gpodder;
in {
  options = {
    programs.gpodder = {
      enable = mkEnableOption "Enable gpodder";
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      gpodder mutagen normalize
    ];

    home.sessionVariables.GPODDER_HOME = "$HOME/var/pod";
  };
}


