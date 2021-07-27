{ config, lib, pkgs, domains, ... }:

let
  inherit (lib) genAttrs const nameValuePair;

  email = "ssl+${config.networking.hostName}@${domains.home}";
  mkCertFor = domain: rec {
    inherit email;
    #inherit domain;
    #directory = "/var/lib/acme/${domain}/";
    extraDomainNames = [ "*.${domain}" ];
    group = "keys";

    dnsProvider = "cloudflare";
    credentialsFile = "/etc/ssl/${dnsProvider}";
    dnsPropagationCheck = true;
  };
in {
  security.acme = {
    certs = genAttrs (builtins.attrValues domains) mkCertFor;
  };

  systemd.services.traefik.serviceConfig = lib.mkIf config.services.traefik.enable {
    SupplementaryGroups = "keys";
  };
}
