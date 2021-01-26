{ config, lib, pkgs, ... }:

with lib;

let cfg = config.services.swc-launch.server.velox;
in {
  options = {
    services.swc-launch.server.velox = {
      enable = mkOption {
        default = false;
        description = "Enable the Velox compositor.";
      };
      command = mkOption {
        internal = true;
        type = types.str;
        description = "Velox compositor command.";
      };
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ velox ];

    services.swc-launch.server = { available_servers = [ "velox" ]; };
    services.swc-launch.server.velox = { command = "${pkgs.velox}/bin/velox"; };
  };
}
