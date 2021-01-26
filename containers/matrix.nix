{ config, pkgs, lib, usr, flake, ... }:

let
  hostAddress = "10.7.0.1";
  localAddress = "10.7.0.2";
in {
  services.postgresql.enable = true;
  services.postgresql.ensureUsers = [
    { name = "matrix-synapse"; ensurePermissions."DATABASE \"matrix-synapse\"" = "ALL PRIVILEGES"; }
  ];
  services.postgresql.ensureDatabases = [ "matrix-synapse" ];

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
            flake.inputs.construct.nixosModules.matrix-construct
          ];

          environment.memoryAllocator.provider = "jemalloc";

          environment.systemPackages = with pkgs; [ matrix-construct screen ];
          systemd.services.matrix-synapse.environment = {
            SYNAPSE_CACHE_FACTOR = "4.0";
          };
          services.matrix-synapse = rec {
            enable = true;
            server_name = "sn.${usr.secrets.domains.srvc}";
            enable_registration = true;
            inherit (usr.secrets.matrix.synapse) registration_shared_secret;
            public_baseurl = "https://matrix.${usr.secrets.domains.srvc}/";
            tls_certificate_path = "/var/lib/acme/${usr.secrets.domains.srvc}/fullchain.pem";
            tls_private_key_path = "/var/lib/acme/${usr.secrets.domains.srvc}/key.pem";
            database_type = "psycopg2";
            database_args = {
              user = "matrix-synapse";
              database = "matrix-synapse";
              host = hostAddress;
            };
            listeners = [
              { # federation
                bind_address = "";
                port = 8448;
                resources = [
                  { compress = true; names = [ "client" "webclient" ]; }
                  { compress = false; names = [ "federation" ]; }
                ];
                tls = true;
                type = "http";
                x_forwarded = false;
              }
              { # client
                bind_address = "0.0.0.0";
                port = 8008;
                resources = [
                  { compress = true; names = [ "client" "webclient" ]; }
                ];
                tls = false;
                type = "http";
                x_forwarded = true;
              }
            ];
            servers = {
              "matrix.org" = { "ed25519:a_RXGa" = "l8Hft5qXKn1vfHrg3p4+W8gELQVo8N13JkluMfmn2sQ"; };
              "privacytools.io" = { "ed25519:a_UqmI" = "NlVbHUvTMqHQmpXCQwEsSwJwzPju1o+xgzeCr92mc04"; };
              "mozilla.org" = { "ed25519:0" = "RsDggkM9GntoPcYySc8AsjvGoD0LVz5Ru/B/o5hV9h4"; };
              "disroot.org" = { "ed25519:a_ngBm" = "GhYGEZEw3s2DjbXThOhqmgntsRmgRYUFrw1i0BYDHJk"; };
              "tchncs.de" = { "ed25519:a_rOPL" = "HZxh/ZZktCgLcsJgKw2tHS9lPcOo1kNBoEdeVtmkpeg"; };
            };
            extraConfig = ''
              enable_group_creation: true
              max_upload_size: "100M"
              use_presence: false
            '';
          };

          services.matrix-construct = {
            enable = true;
            useScreen = false;
            server = "cs.${usr.secrets.domains.srvc}";
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

          users.users.matrix-synapse.extraGroups = [
            "keys"
          ];
         #users.users.construct.extraGroups = [
         #  "keys"
         #];
        };
      bindMounts = {
        "/var/lib/matrix-synapse" = {
          hostPath = "/var/lib/synapse";
          isReadOnly = false;
        };
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
