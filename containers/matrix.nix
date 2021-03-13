{ config, pkgs, lib, usr, flake, ... }:

let
  hostAddress = "10.7.0.1";
  localAddress = "10.7.0.2";
in {
  services.postgresql = {
    enable = true;
    ensureUsers = [{
      name = "dendrite";
      ensurePermissions."DATABASE \"dendrite\"" = "ALL PRIVILEGES";
    }];
    ensureDatabases = [ "dendrite" ];
  };

  containers.matrix =
    {
      autoStart = true;
      enableTun = true;
      privateNetwork = true;
      inherit hostAddress localAddress;

      config =
        { ... }:

        {
          #environment.memoryAllocator.provider = "jemalloc";

          nixpkgs = { inherit pkgs; };

          environment.systemPackages = with pkgs; [ screen jq vim ipfs ipfscat ];
          environment.variables = {
            IPFS_PATH = "/var/lib/ipfs";
          };

          services.matrix-dendrite = rec {
            enable = true;
            generatePrivateKey = true;
            generateTls = false;
            httpPort = 8008;
            httpsPort = 8448;
            settings = let
              mkDb = with {
                login = "dendrite";
                hostname = hostAddress;
                database = "dendrite";
                args = "sslmode=disable";
              }; name: "postgresql://${login}@${hostname}/${database}?${args}";
            in {
              global.server_name = "${usr.secrets.domains.srvc}";
              global.disable_federation = false;
              global.kafka.use_naffka = true;
              global.kafka.topic_prefix = "Dendrite";
              global.kafka.naffka_database.connection_string = mkDb "naffka";
              app_service_api.database.connection_string = mkDb "appservice";
              federation_sender.database.connection_string = mkDb "federationsender";
              key_server.database.connection_string = mkDb "keyserver";
              media_api.database.connection_string = mkDb "mediaapi";
              mscs.database.connection_string = mkDb "mscs";
              room_server.database.connection_string = mkDb "roomserver";
              signing_key_server.database.connection_string = mkDb "signingkeyserver";
              signing_key_server.prefer_direct_fetch = false;
              signing_key_server.key_perspectives = [{
                server_name = "matrix.org";
                keys = [{
                  key_id = "ed25519:auto";
                  public_key = "Noi6WqcDj0QmPxCNQqgezwTlBKrfqehY1u2FyWP9uYw";
                } {
                  key_id = "ed25519:a_RXGa";
                  public_key = "l8Hft5qXKn1vfHrg3p4+W8gELQVo8N13JkluMfmn2sQ";
                }];
              }];
              sync_api.database.connection_string = mkDb "syncapi";
              sync_api.real_ip_header = "X-Real-IP";
              user_api.account_database.connection_string = mkDb "userapi-accounts";
              user_api.device_database.connection_string = mkDb "userapi-devices";
              client_api = {
                registration_disabled = false;
                inherit (usr.secrets.matrix.synapse) registration_shared_secret;
              };
              mscs.mscs = [ "msc2946" ];
              logging = [{
                type = "file";
                level = "debug";
                params.path = "/var/lib/matrix-dendrite/log";
              }];
            };
            tlsCert = "/var/lib/acme/${usr.secrets.domains.srvc}/fullchain.pem";
            tlsKey = "/var/lib/acme/${usr.secrets.domains.srvc}/key.pem";
          };

          services.nginx.enable = true;
          services.nginx.virtualHosts.wellknown-matrix = {
            locations = {
              "/server".extraConfig = ''
                return 200 '{ "m.server": "m.${usr.secrets.domains.srvc}:443" }';
              '';
              "/client".extraConfig = ''
                return 200 '{ "m.homeserver": { "base_url": "https://m.${usr.secrets.domains.srvc}" } }';
              '';
            };
          };

          systemd.services.matrix-dendrite = {
            serviceConfig.Group = "keys";
          };

          networking.firewall.enable = false;

         #users.users.construct.extraGroups = [
         #  "keys"
         #];
        };
      bindMounts = {
        "/var/lib/private/matrix-dendrite" = {
          hostPath = "/var/lib/dendrite";
          isReadOnly = false;
        };
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
        "/var/lib/ipfs" = {
          hostPath = "/var/lib/ipfs";
          isReadOnly = true;
        };
        "/run/ipfs.sock" = {
          hostPath = "/run/ipfs.sock";
          isReadOnly = false;
        };
      };
    };
}
