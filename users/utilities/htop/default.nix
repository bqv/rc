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


      fields = [
        "PID"
        "USER"   
        "NICE"   
        "STATE"   
        "PRIORITY"
        "PERCENT_CPU"  
        "M_RESIDENT"
        "PERCENT_MEM" 
        "IO_RATE"
        "STARTTIME"
        "COMM"
      ];

      hideThreads = false;
      hideKernelThreads = false;
      hideUserlandThreads = true;
    };
  };
}
