{ config ? {}, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.htop;
in {
  config = mkIf cfg.enable {
    programs.htop = {
      headerMargin = false;
      meters = {
        left = [ "LeftCPUs2" "Memory" "Swap" "Hostname" ];
        right = [ "RightCPUs2" "Tasks" "LoadAverage" "Uptime" ];
      };

      treeView = true;
      showProgramPath = false;

      hideThreads = false;
      hideKernelThreads = false;
      hideUserlandThreads = true;
    };
  };
}
