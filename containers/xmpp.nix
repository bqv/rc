{ config, pkgs, lib, usr, flake, ... }:

let
  hostAddress = "10.7.0.1";
  localAddress = "10.7.0.2";
in {
 #services.postgresql = {
 #  enable = true;
 #  ensureUsers = [{
 #    name = "prosody";
 #    ensurePermissions."DATABASE \"prosody\"" = "ALL PRIVILEGES";
 #  }];
 #  ensureDatabases = [ "prosody" ];
 #};

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
            IPFS_PATH = "${pkgs.runCommand "ipfs-path" {
              api = "/ip4/${usr.secrets.hosts.wireguard.ipv4.zeta}/tcp/5001";
              passAsFile = [ "api" ];
            } ''
              mkdir $out
              ln -s $apiPath $out/api
            ''}";
          };

          services.prosody = rec {
            enable = true;
            admins = [ "bqv@jix.im" ];
            allowRegistration = true;
            httpPorts = [ 5280 ];
            httpsPorts = [ 5281 ];
            group = "keys";
            modules.admin_adhoc = true;
            modules.admin_telnet = true;
            modules.bosh = true;
            modules.groups = true;
            modules.legacyauth = true;
            modules.websocket = true;
            muc = [{
              domain = "muc.xa0.uk";
              maxHistoryMessages = 10000;
              name = "Zeta Prosody";
            }];
            ssl.cert = "/var/lib/acme/${usr.secrets.domains.srvc}/fullchain.pem";
            ssl.key = "/var/lib/acme/${usr.secrets.domains.srvc}/key.pem";
            uploadHttp = {
              domain = "xmpp.xa0.uk";
            };
            disco_items = "xmpp.xa0.uk";
            virtualhosts = "xmpp.xa0.uk";
          };

          networking.firewall.enable = false;
        };
      bindMounts = {
        "/var/lib/prosody" = {
          hostPath = "/var/lib/prosody";
          isReadOnly = false;
        };
        "/var/lib/acme" = {
          hostPath = "/var/lib/acme";
          isReadOnly = true;
        };
      };
    };
}
