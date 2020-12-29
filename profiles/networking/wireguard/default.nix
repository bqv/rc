{ config, lib, hosts, ... }:

let
  network = 24;
  peers = {
    zeta = {
      ip = hosts.wireguard.zeta;
      wideArea = [ hosts.ipv4.zeta ]; # Note: Wireguard won't retry DNS resolution if it fails
      publicKey = "WbZqPcgSxWf+mNsWVbS+0JylysN9FKrRG9783wn1JAg=";
    };

    theta = {
      ip = hosts.wireguard.theta;
      routes.zeta = [ "10.0.0.0/24" ];
      publicKey = "Itld9S83/URY8CR1ZsIfYRGK74/T0O5YbsHWcNpn2gE=";
    };

    delta = {
      ip = hosts.wireguard.delta;
      wideArea = [ hosts.ipv4.home ];
      localArea = [ hosts.lan.delta-wired hosts.lan.delta-wireless ];
      publicKey = "Y/SRDGEQfFLUGqx6vMnO1pxHs9zn//NpwdSGQ2Sm+Dg=";
    };

    phi = {
      ip = hosts.wireguard.phi;
      localArea = [ hosts.lan.phi ];
      publicKey = "kccZA+GAc0VStb28A+Kr0z8iPCWsiuRMfwHW391Qrko=";
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
      ips = [ "${currentPeer.ip}/${toString network}" ];
      privateKeyFile = config.secrets.files.wireguard.file;
      generatePrivateKeyFile = false;
      listenPort = currentPeer.port or 51820;

      peers = lib.mapAttrsToList (hostname: hostcfg: {
        inherit (hostcfg) publicKey;
        allowedIPs = [ "${hostcfg.ip}/32" ]
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
      file = ../../secrets/keys/wireguard + "/${config.networking.hostName}.key";
     #user = "root";
     #group = "root";
    };
  };
}
