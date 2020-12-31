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
              yq yj jq git
            ];

            services.jellyfin.enable = true;

            services.sonarr.enable = true;
            services.radarr.enable = true;
            services.lidarr.enable = true;
            services.bazarr.enable = true;
            services.jackett.enable = true;

            services.transmission = {
              enable = true;
              settings.download-dir = "/srv/ftp";
            };
          };
        };
      bindMounts = {
        "/srv/ftp" = {
          hostPath = "/srv/ftp";
          isReadOnly = false;
        };
      };
    };

  system.activationScripts.srv-ftp = ''
    mkdir -p /srv/ftp
  '';
}
