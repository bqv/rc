{ config, pkgs, lib, domains, inputs, ... }:

let
  hostAddress = "10.7.0.1";
  localAddress = "10.7.0.2";
in {
  containers.matrix =
    {
      autoStart = true;
      enableTun = true;
      privateNetwork = true;
      inherit hostAddress localAddress;

      config =
        { ... }:
 
        {
          imports = [
            inputs.construct.nixosModules.matrix-construct
          ];

          environment.memoryAllocator.provider = "jemalloc";

          services.matrix-construct = {
            enable = true;
            useScreen = false;
            server = "cs.${domains.srvc}";
            package = pkgs.matrix-construct.overrideAttrs (_: {
              doInstallCheck = true;
            });
          };

          systemd.services.restart-construct = {
            serviceConfig = {
              Type = "oneshot";
              ExecStart = "systemctl restart matrix-construct.service";
            };
          };
          systemd.timers.restart-construct = {
            timerConfig = {
              OnStartupSec = "1d";
              OnUnitActiveSec = "1d";
            };
            wantedBy = [ "timers.target" ];
          };

          networking.firewall.enable = false;

         #users.users.construct.extraGroups = [
         #  "keys"
         #];
        };
      bindMounts = {
        "/var/lib/construct" = {
          hostPath = "/var/lib/construct";
          isReadOnly = false;
        };
        "/var/log/construct" = {
          hostPath = "/var/log/construct";
          isReadOnly = false;
        };
        "/var/lib/acme" = {
          hostPath = "/var/lib/acme";
          isReadOnly = true;
        };
      };
    };
}
