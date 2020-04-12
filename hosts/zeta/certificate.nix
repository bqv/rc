{ config, lib, pkgs, ... }:

let
  inherit (lib) genAttrs const nameValuePair;

  email = "ssl+${config.networking.hostName}@***REMOVED***";
  domains = [
    "***REMOVED***"
    "***REMOVED***"
    "***REMOVED***"
    "***REMOVED***"
  ];
  mkCertFor = domain: rec {
    inherit email;
    allowKeysForGroup = true;
    #directory = "/var/lib/acme/${domain}/";
    #domain = domain;
    extraDomains = genAttrs [
      "*.${domain}"
    ] (const null);
    group = "keys";

    dnsProvider = "cloudflare";
    credentialsFile = "/etc/ssl/${dnsProvider}";
    dnsPropagationCheck = true;
  };
in {
  security.acme = {
    certs = genAttrs domains mkCertFor;
  };

  systemd.services.haproxy.serviceConfig = {
    SupplementaryGroups = "keys";
  };
}
