{ config, lib, pkgs, ... }:

with lib;

let cfg = config.services.swc-launch.server.velox;
in {
  ###### interface
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

  ###### implementation
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ velox ];

    services.swc-launch.server = { available_servers = [ "velox" ]; };
    services.swc-launch.server.velox = { command = "${pkgs.velox}/bin/velox"; };

   #services.swc-launch.server.velox = { command = "${config.security.wrapperDir}/velox"; };
   #security.wrappers.velox = {
   #  source = "${pkgs.velox}/bin/velox";
   #  owner = "root";
   #  group = "root";
   #  setuid = true;
   #};
  };
}
