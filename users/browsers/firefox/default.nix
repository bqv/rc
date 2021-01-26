{ config, lib, pkgs, ... }:

with lib; let
  cfg = config.programs.firefox;
in {
  config = mkIf cfg.enable {
   #programs.firefox.package = pkgs.firefox;#builtins.trace "pkgs.firefox: broken?" pkgs.hello;
  };
}
