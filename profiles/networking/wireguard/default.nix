{ config, lib, hosts, ... }:

let
  network = 24;
  peers = {
    zeta = {
      ip = hosts.wireguard.zeta;

      # Note: Wireguard won't retry DNS resolution if it fails
      endpoint = hosts.ipv4.zeta;
      port = 51820;
      publicKey = "WbZqPcgSxWf+mNsWVbS+0JylysN9FKrRG9783wn1JAg=";
    };

    theta = {
      ip = hosts.wireguard.theta;
      #endpoint = "0.0.0.0";
      port = 51820;
      publicKey = "Itld9S83/URY8CR1ZsIfYRGK74/T0O5YbsHWcNpn2gE=";
    };

    delta = {
      ip = hosts.wireguard.delta;
      #endpoint = "0.0.0.0";
      port = 51820;
      publicKey = "Y/SRDGEQfFLUGqx6vMnO1pxHs9zn//NpwdSGQ2Sm+Dg=";
    };

    mu = {
      ip = hosts.wireguard.mu;
      endpoint = "192.168.0.19";
      port = 51820;
      publicKey = "0000000000000000000000000000000000000000000=";
    };

    nu = {
      ip = hosts.wireguard.nu;
      endpoint = "192.168.0.15";
      port = 51820;
      publicKey = "0000000000000000000000000000000000000000000=";
    };
  };

  currentPeer = peers."${config.networking.hostName}";
  peerable = from: to: (from != to) && (peers."${to}" ? "publicKey");
in {
  networking.firewall.allowedUDPPorts =
    lib.mkIf (currentPeer ? "port") [ currentPeer.port ];

  networking.wg-quick = { };

  networking.wireguard = {
    enable = true;
    interfaces.wg0 = {
      ips = [ "${currentPeer.ip}/${toString network}" ];
      privateKeyFile = "/etc/wireguard/private.key";
      generatePrivateKeyFile = true;
      listenPort = currentPeer.port or null;
  
      peers = lib.mapAttrsToList (hostname: hostcfg: {
        inherit (hostcfg) publicKey;
        allowedIPs = [ "${hostcfg.ip}/32" ];
      } // (lib.optionalAttrs (hostcfg ? "endpoint") {
        endpoint = "${hostcfg.endpoint}:${toString hostcfg.port}";
        persistentKeepalive = 60;
      })) (lib.filterAttrs (hostname: _:
        peerable config.networking.hostName hostname
      ) peers);
    };
  };
}
