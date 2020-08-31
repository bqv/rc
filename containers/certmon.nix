{ config, pkgs, lib, ... }:

{
  options.ssl.servers = lib.mkOption {
    type = with lib.types; listOf string;
    default = [
      "sso.${domains.home}"
      "torrent.${domains.home}"
      "search.${domains.home}"
      "ca.${domains.home}"
      domains.srvc
      "u.${domains.srvc}"
      "ublog.${domains.srvc}"
      "microblog.${domains.srvc}"
      "mastodon.${domains.srvc}"
      "matrix.${domains.srvc}"
    ];
  };
  config = {
    security.acme.certs = let 
      mkCert = host: {
        name = host;
        value = {
          allowKeysForGroup = true;
         #directory = "/var/lib/acme/${host}/";
         #domain = host;
          email = "ssl@${domains.home}";
          extraDomains = { "www.${host}" = null; };
          group = "keys";
         #plugins = [ "fullchain.pem" "full.pem" "key.pem" "account_key.json" "account_reg.json" ]
         ## "cert.der", "cert.pem", "chain.pem", "external.sh", "key.der"
          postRun = "systemctl restart traefik";
          webroot = "/var/www/${host}/";
        };
      };
    in builtins.listToAttrs (map mkCert config.ssl.servers);

    systemd.services.haproxy.serviceConfig = lib.mkIf config.services.haproxy.enable {
      SupplementaryGroups = "keys";
    };

    containers.certmon = {
      autoStart = true;
      enableTun = true;
      privateNetwork = true;
      hostAddress = "10.3.0.1";
      localAddress = "10.3.0.2";
      config =
        { config, ... }:
        {
          services.nginx = {
            enable = true;
            group = "root";
            virtualHosts = let
              mkHost = host: {
                name = host;
                value = {
                  serverName = host;
                  serverAliases = [ "www.${host}" ];
                  root = "/var/www/${host}/";
                };
              };
            in builtins.listToAttrs (map mkHost config.ssl.servers);
          };

          networking.firewall.enable = false;
        };
      bindMounts = {
        "/var/www" = {
          hostPath = "/var/www";
          isReadOnly = true;
        };
      };
    };
  };
}
