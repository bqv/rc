{ config, pkgs, lib, usr, flake, ... }:

let
  hostAddress = "10.7.0.1";
  localAddress = "10.7.0.2";
in {
  services.postgresql = {
    enable = true;
    ensureUsers = [{
      name = "prosody";
      ensurePermissions."DATABASE \"prosody\"" = "ALL PRIVILEGES";
    }];
    ensureDatabases = [ "prosody" ];
  };

  containers.xmpp =
    {
      autoStart = true;
      enableTun = true;
      privateNetwork = true;
      inherit hostAddress localAddress;

      config =
        { ... }:

        {
          nixpkgs = { inherit pkgs; };

          environment.systemPackages = with pkgs; [ jq vim ipfs ipfscat ];
          environment.variables = {
            IPFS_PATH = pkgs.runCommand "ipfs-path" {
              api = "/ip4/${usr.secrets.hosts.wireguard.ipv4.zeta}/tcp/5001";
              passAsFile = [ "api" ];
            } ''
              mkdir $out
              ln -s $apiPath $out/api
            '';
          };

          services.prosody = rec {
            enable = true;
            admins = [ "bqv@jix.im" ];
            allowRegistration = true;
            admin_adhoc = true;
            admin_telnet = true;
            httpPorts = [ 5280 ];
            httpsPorts = [ 5281 ];
            bosh = true;
            modules.legacyauth = true;
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
        };
      bindMounts = {
        "/var/lib/acme" = {
          hostPath = "/var/lib/acme";
          isReadOnly = true;
        };
      };
    };
}
