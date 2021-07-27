{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.htop;
in {
  config = mkIf cfg.enable {
    programs.htop = {
      settings = {
        header_margin = false;
        left_meters = [ "LeftCPUs2" "Memory" "Swap" "Hostname" ];
        right_meters = [ "RightCPUs2" "Tasks" "LoadAverage" "Uptime" ];

        tree_view = true;
        show_program_path = false;


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

        hide_threads = false;
        hide_kernel_threads = false;
        hide_userland_threads = true;
      };
    };
  };
}
