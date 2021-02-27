{ config, pkgs, lib, domains, ... }:

let
  hostAddress = "10.11.0.1";
  localAddress = "10.11.0.2";

  hostConfig = config;
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
              settings.dht-enabled = true;
              settings.download-dir = "/srv/ftp/torrents";
              settings.download-queue-enabled = false;
              settings.rpc-authentication-required = false;
              settings.rpc-bind-address = "0.0.0.0";
              settings.rpc-host-whitelist-enabled = false;
              settings.rpc-whitelist-enabled = false;
            };
            systemd.services.transmission = {
              environment.TRANSMISSION_WEB_HOME = pkgs.fetchFromGitHub {
                owner = "Secretmapper";
                repo = "combustion";
                rev = "d5ff91d4078b41bd3738542a20d802cd3ff6cc1e";
                sha256 = "gZ4YOKMsnYEWDLnh8OZNwEg1ZJioZsWrOcAjHLIyFYg=";
              };
            };

            users = rec {
              users = {
                root.extraGroups = [ "transmission" ];

                jellyfin.extraGroups = [ "transmission" ];
                sonarr.extraGroups = [ "transmission" ];
                radarr.extraGroups = [ "transmission" ];
                lidarr.extraGroups = [ "transmission" ];
                bazarr.extraGroups = [ "transmission" ];
                jackett.extraGroups = [ "transmission" ];

                transmission = lib.mapAttrs (_: lib.mkForce) {
                  inherit (hostConfig.users.users.bao) uid group;
                } // {
                  extraGroups = [ "transmission" ];
                };
              };
            };
          };
        };
      bindMounts = {
        "/srv/ftp/tv" = {
          hostPath = "/srv/ftp/tv";
          isReadOnly = false;
        };
        "/srv/ftp/TV" = {
          hostPath = "/srv/ftp/tv";
          isReadOnly = false;
        };
        "/srv/ftp/TV Shows" = {
          hostPath = "/srv/ftp/tv";
          isReadOnly = false;
        };
        "/srv/ftp/movies" = {
          hostPath = "/srv/ftp/movies";
          isReadOnly = false;
        };
        "/srv/ftp/Movies" = {
          hostPath = "/srv/ftp/movies";
          isReadOnly = false;
        };
        "/srv/ftp/books" = {
          hostPath = "/srv/ftp/books";
          isReadOnly = false;
        };
        "/srv/ftp/Books" = {
          hostPath = "/srv/ftp/books";
          isReadOnly = false;
        };
        "/srv/ftp/torrents" = {
          hostPath = "/srv/ftp";
          isReadOnly = false;
        };
        "/srv/ftp/Torrents" = {
          hostPath = "/srv/ftp";
          isReadOnly = false;
        };
      };
    };

  system.activationScripts.srv-ftp = ''
    mkdir -p /srv/ftp
    chmod g+rwx /srv/ftp
  '';

  systemd.tmpfiles.rules = [
    "d /srv/ftp 2777 root root"
  ];
}
