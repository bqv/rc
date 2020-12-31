{ config, pkgs, lib, domains, ... }:

let
  hostAddress = "10.11.0.1";
  localAddress = "10.11.0.2";
in {
  containers.jellyfin =
    {
      autoStart = true;
      enableTun = true;
      privateNetwork = true;
      inherit hostAddress localAddress;

      config =
        { config, stdenv, ... }:

        {
          config = {
            nixpkgs.pkgs = pkgs;

            networking.firewall.enable = false;

            environment.systemPackages = with pkgs; [
              vervis yq yj jq git darcs
            ];

            services.jellyfin.enable = true;
          };
        };
      bindMounts = {
        "/var/lib/jellyfin" = {
          hostPath = "/var/lib/jellyfin";
          isReadOnly = false;
        };
      };
    };

  system.activationScripts.var-lib-jellyfin = ''
    mkdir -p /var/lib/jellyfin
  '';
}
