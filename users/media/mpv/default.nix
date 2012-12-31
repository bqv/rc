{ config, lib, pkgs, ... }:

with lib; let
  cfg = config.programs.mpv;
in {
  config = mkIf cfg.enable {
    programs.mpv.config = {
      vo = "gpu";
      gpu-context = "wayland";
    };
  };
}
