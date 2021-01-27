{ config, lib, usr, hosts, ... }:

let
  pubkeys = usr.secrets.wireguard.pubkeys;

  network = 24;
  peers = {
    zeta = {
      ip = hosts.wireguard.zeta;
      ip6 = hosts.wireguard6.zeta;
      wideArea = [ hosts.ipv4.zeta ]; # Note: Wireguard won't retry DNS resolution if it fails
      publicKey = pubkeys.zeta;
    };

    theta = {
      ip = hosts.wireguard.theta;
      ip6 = hosts.wireguard6.theta;
      routes.zeta = [ "10.0.0.0/24" ];
      publicKey = pubkeys.theta;
    };

    delta = {
      ip = hosts.wireguard.delta;
      ip6 = hosts.wireguard6.delta;
      wideArea = [ hosts.ipv4.home ];
      localArea = [ hosts.lan.delta-wired hosts.lan.delta-wireless ];
      publicKey = pubkeys.delta;
    };

    phi = {
      ip = hosts.wireguard.phi;
      ip6 = hosts.wireguard6.phi;
      localArea = [ hosts.lan.phi ];
      publicKey = pubkeys.phi;
    };
  };

  currentPeer = peers."${config.networking.hostName}";
  isLan = peer: builtins.length (peers.${peer}.localArea or []) > 0;
  isWan = peer: !(isLan peer);
  endpointsOf = peer: (peer.wideArea or []) ++ (peer.localArea or []);
  peerable = from: to: builtins.all lib.id [
    (from != to)
    (peers.${to} ? publicKey)
  ];
in {
  environment.etc."wireguard/private.key".source = config.secrets.files.wireguard.file;

  networking.firewall.allowedUDPPorts =
    lib.mkIf (currentPeer ? "port") [ currentPeer.port ];

  networking.wg-quick = { };

  networking.wireguard = {
    enable = true;
    interfaces.wg0 = {
      ips = [
        "${currentPeer.ip}/${toString network}"
        "${currentPeer.ip6}/${toString network}"
      ];
      privateKeyFile = "${config.secrets.files.wireguard.file}";
      generatePrivateKeyFile = false;
      listenPort = currentPeer.port or 51820;

      peers = lib.mapAttrsToList (hostname: hostcfg: {
        inherit (hostcfg) publicKey;
        allowedIPs = [ "${hostcfg.ip}/32" "${hostcfg.ip6}/32" ]
          ++ (hostcfg.routes.${config.networking.hostName} or []);
      } // (lib.optionalAttrs (builtins.length (endpointsOf hostcfg) > 0) {
        endpoint = "${builtins.head (endpointsOf hostcfg)}:${toString hostcfg.port or "51820"}";
        persistentKeepalive = 30;
      })) (lib.filterAttrs (hostname: _:
        peerable config.networking.hostName hostname
      ) peers);
    };
  };

  systemd.services.wireguard-wg0.serviceConfig.Before = [ "sshd.service" ];

  secrets.files = {
    wireguard = {
      file = usr.secrets.keyDir + "/wireguard/${config.networking.hostName}.key";
     #user = "root";
     #group = "root";
    };
  };
}
