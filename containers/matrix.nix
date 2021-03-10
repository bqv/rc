{ config, pkgs, lib, usr, flake, ... }:

let
  hostAddress = "10.7.0.1";
  localAddress = "10.7.0.2";
in {
  services.postgresql = let
    databases = [
      "naffka"
      "appservice"
      "federationsender"
      "keyserver"
      "mediaapi"
      "mscs"
      "roomserver"
      "signingkeyserver"
      "syncapi"
      "userapi-accounts"
      "userapi-devices"
    ];
  in {
    enable = true;
    ensureUsers = map (x: {
      name = "dendrite";
      ensurePermissions."DATABASE \"dendrite-${x}\"" = "ALL PRIVILEGES";
    }) databases;
    ensureDatabases = map (x: "dendrite-${x}") databases;
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

          environment.systemPackages = with pkgs; [ screen ];
          services.matrix-dendrite = rec {
            enable = true;
            #environmentFile = null;
            generatePrivateKey = true;
            generateTls = false;
            httpPort = 8008;
            settings = let
              mkDb = with {
                authority = "dendrite";
                hostname = hostAddress;
              }; name: "postgresql://${authority}@${hostname}/dendrite-${name}?sslmode=disable";
            in {
              global.server_name = "${usr.secrets.domains.srvc}:${toString httpPort}";
              global.kafka.naffka_database.connection_string = mkDb "naffka";
              app_service_api.database.connection_string = mkDb "appservice";
              federation_sender.database.connection_string = mkDb "federationsender";
              key_server.database.connection_string = mkDb "keyserver";
              media_api.database.connection_string = mkDb "mediaapi";
              mscs.database.connection_string = mkDb "mscs";
              room_server.database.connection_string = mkDb "roomserver";
              signing_key_server.database.connection_string = mkDb "signingkeyserver";
              sync_api.database.connection_string = mkDb "syncapi";
              user_api.account_database.connection_string = mkDb "userapi-accounts";
              user_api.device_database.connection_string = mkDb "userapi-devices";
              client_api = {
                inherit (usr.secrets.matrix.synapse) registration_shared_secret;
                registration_disabled = false;
              };
              mscs.mscs = [ "msc2946" ];
             #public_baseurl = "https://matrix.${usr.secrets.domains.srvc}/";
            };
            tlsCert = "/var/lib/acme/${usr.secrets.domains.srvc}/fullchain.pem";
            tlsKey = "/var/lib/acme/${usr.secrets.domains.srvc}/key.pem";
          };

          systemd.services.matrix-dendrite = let
            cfg = config.services.matrix-dendrite;
          in {
            serviceConfig.Group = "keys";
            serviceConfig.ExecStart = lib.strings.concatStringsSep " " ([
              "${pkgs.matrix-dendrite}/bin/dendrite-monolith-server"
              "--config /run/matrix-dendrite/dendrite.yaml"
            ] ++ lib.optionals (cfg.httpPort != null) [
              "--http-bind-address :${builtins.toString cfg.httpPort}"
            ] ++ lib.optionals (cfg.httpsPort != null) [
              "--https-bind-address :${builtins.toString cfg.httpsPort}"
            ] ++ lib.optionals (cfg.tlsCert != null && cfg.tlsKey != null) [
              "--tls-cert ${cfg.tlsCert}"
              "--tls-key ${cfg.tlsKey}"
            ]);
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
      };
    };
}
