{ config, lib, ... }:

let
  network = 24;
  hosts = {
    zeta = {
      ip = "10.0.0.1";

      # Note: Wireguard won't retry DNS resolution if it fails
      endpoint = "163.172.7.233";
      port = 51820;
      publicKey = "WbZqPcgSxWf+mNsWVbS+0JylysN9FKrRG9783wn1JAg=";
    };

    theta = {
      ip = "10.0.0.2";
      #endpoint = "46.4.66.184";
      port = 51820;
      publicKey = "0000000000000000000000000000000000000000000=";
    };

    delta = {
      ip = "10.0.0.3";
      #endpoint = "46.4.66.184";
      port = 51820;
      publicKey = "Y/SRDGEQfFLUGqx6vMnO1pxHs9zn//NpwdSGQ2Sm+Dg=";
    };

    mu = {
      ip = "10.0.0.4";
      endpoint = "192.168.0.19";
      port = 51820;
      publicKey = "0000000000000000000000000000000000000000000=";
    };

    nu = {
      ip = "10.0.0.5";
      endpoint = "192.168.0.19";
      port = 51820;
      publicKey = "0000000000000000000000000000000000000000000=";
    };
  };

  currentHost = hosts."${config.networking.hostName}";
  peerable = from: to: (from != to) && (to ? "publicKey");
in {
  networking.firewall.allowedUDPPorts =
    lib.mkIf (currentHost ? "port") [ currentHost.port ];

  networking.wg-quick = { };

  networking.wireguard = {
    enable = true;
    interfaces.wg0 = {
      ips = [ "${currentHost.ip}/${toString network}" ];
      privateKeyFile = "/etc/wireguard/private.key";
      generatePrivateKeyFile = true;
      listenPort = currentHost.port or null;
  
      peers = lib.mapAttrsToList (hostname: hostcfg: {
        inherit (hostcfg) publicKey;
        allowedIPs = [ "${hostcfg.ip}/32" ];
      } // (lib.optionalAttrs (hostcfg ? "endpoint") {
        endpoint = "${hostcfg.endpoint}:${toString hostcfg.port}";
        persistentKeepalive = 60;
      })) (lib.filterAttrs (hostname: hostcfg:
        peerable config.networking.hostName hostname
      ) hosts);
    };
  };
}
