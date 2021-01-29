{ config, pkgs, lib, usr, hosts, ... }:

let
  pubkeys = usr.secrets.wireguard.pubkeys;
  iptables = pkgs.iptables-nftables-compat;

  network = 16;#24;
  network6 = 112;
  peers = { # Note: Wireguard won't retry DNS resolution if it fails
    zeta = {
      zeta = rec {
        ipv4 = hosts.wireguard.ipv4.zeta;
        ipv6 = hosts.wireguard.ipv6.zeta;
        wideArea4 = [ hosts.ipv4.zeta.address ];
        wideArea6 = [ "${hosts.ipv6.zeta.prefix}:1" ];
        publicKey = pubkeys.zeta;
      };

      theta = rec {
        ipv4 = hosts.wireguard.ipv4.theta;
        ipv6 = hosts.wireguard.ipv6.theta;
        routes4.zeta = [ "${hosts.wireguard.ipv4.theta}/24" ];
        routes6.zeta = [ "${hosts.wireguard.ipv6.theta}/112" ];
        publicKey = pubkeys.theta;
      };

      delta = rec {
        ipv4 = hosts.wireguard.ipv4.delta;
        ipv6 = hosts.wireguard.ipv6.delta;
        wideArea4 = [ hosts.ipv4.r-home.address ];
        wideArea6 = [ hosts.ipv6.r-home.address ];
        localArea = [ hosts.lan.delta-wired hosts.lan.delta-wireless ];
        publicKey = pubkeys.delta;
      };

      phi = rec {
        ipv4 = hosts.wireguard.ipv4.phi;
        ipv6 = hosts.wireguard.ipv6.phi;
        localArea = [ hosts.lan.phi ];
        publicKey = pubkeys.phi;
      };
    };
    theta = {
      zeta = {
        ip = hosts.wireguard.zeta;
        ip6 = hosts.wireguard6.zeta;
        wideArea = [ hosts.ipv4.zeta ]; # Note: Wireguard won't retry DNS resolution if it fails
        wideArea6 = [ hosts.ipv6.zeta ]; # Note: Wireguard won't retry DNS resolution if it fails
        publicKey = pubkeys.zeta;
      };

      theta = {
        ip = hosts.wireguard.theta;
        ip6 = hosts.wireguard6.theta;
        routes.zeta = [ hosts.cidr.ipv4 ];
        routes6.zeta = [ hosts.cidr.ipv6 ];
        publicKey = pubkeys.theta;
      };

      delta = {
        ip = hosts.wireguard.delta;
        ip6 = hosts.wireguard6.delta;
        wideArea = [ hosts.ipv4.home ];
        wideArea6 = [ hosts.ipv6.home ];
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
    delta = {
      zeta = {
        ip = hosts.wireguard.zeta;
        ip6 = hosts.wireguard6.zeta;
        wideArea = [ hosts.ipv4.zeta ]; # Note: Wireguard won't retry DNS resolution if it fails
        wideArea6 = [ hosts.ipv6.zeta ]; # Note: Wireguard won't retry DNS resolution if it fails
        publicKey = pubkeys.zeta;
      };

      theta = {
        ip = hosts.wireguard.theta;
        ip6 = hosts.wireguard6.theta;
        routes.zeta = [ hosts.cidr.ipv4 ];
        routes6.zeta = [ hosts.cidr.ipv6 ];
        publicKey = pubkeys.theta;
      };

      delta = {
        ip = hosts.wireguard.delta;
        ip6 = hosts.wireguard6.delta;
        wideArea = [ hosts.ipv4.home ];
        wideArea6 = [ hosts.ipv6.home ];
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
    phi = {
      zeta = {
        ip = hosts.wireguard.zeta;
        ip6 = hosts.wireguard6.zeta;
        wideArea = [ hosts.ipv4.zeta ]; # Note: Wireguard won't retry DNS resolution if it fails
        wideArea6 = [ hosts.ipv6.zeta ]; # Note: Wireguard won't retry DNS resolution if it fails
        publicKey = pubkeys.zeta;
      };

      theta = {
        ip = hosts.wireguard.theta;
        ip6 = hosts.wireguard6.theta;
        routes.zeta = [ hosts.cidr.ipv4 ];
        routes6.zeta = [ hosts.cidr.ipv6 ];
        publicKey = pubkeys.theta;
      };

      delta = {
        ip = hosts.wireguard.delta;
        ip6 = hosts.wireguard6.delta;
        wideArea = [ hosts.ipv4.home ];
        wideArea6 = [ hosts.ipv6.home ];
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
  };

  currentPeer = peers."${config.networking.hostName}";
  isLan = peer: builtins.length (peers.${peer}.localArea or []) > 0;
  isLan6 = peer: builtins.length (peers.${peer}.localArea6 or []) > 0;
  isWan = peer: !(isLan peer);
  isWan6 = peer: !(isLan6 peer);
  endpointsOf = peer: (peer.wideArea or []) ++ (peer.localArea or []);
  endpointsOf6 = peer: (peer.wideArea6 or []) ++ (peer.localArea6 or []);
  peerable = from: to: builtins.all lib.id [
    (from != to)
    (peers.${to} ? publicKey)
  ];
in {
  environment.systemPackages = [ pkgs.wgvanity ];
  environment.etc."wireguard/private.key".source = config.secrets.files.wireguard.file;

  networking.firewall.checkReversePath = "loose";
  networking.firewall.allowedUDPPorts =
    lib.mkIf (currentPeer ? "port") [ currentPeer.port ];

  networking.wireguard = {
    enable = true;
    interfaces.wg0 = {
      ips = [
        "${currentPeer.ip}/${toString network}"
      ];
      privateKeyFile = "${config.secrets.files.wireguard.file}";
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

      # Allow wireguard to route traffic to the internet
      postUp = ''
        ${iptables}/bin/iptables -A FORWARD -i wg0 -j ACCEPT
        ${iptables}/bin/iptables -t nat -A POSTROUTING -s ${currentPeer.ip}/24 -o eno1 -j MASQUERADE
      '';

      # Undo the above
      preDown = ''
        ${iptables}/bin/iptables -D FORWARD -i wg0 -j ACCEPT
        ${iptables}/bin/iptables -t nat -D POSTROUTING -s ${currentPeer.ip}/24 -o eno1 -j MASQUERADE
      '';
    };
    interfaces.wg0v6 = {
      ips = [
        "${currentPeer.ip6}/${toString network6}"
      ];
      privateKeyFile = "${config.secrets.files.wireguard.file}";
      generatePrivateKeyFile = false;
      listenPort = currentPeer.port or 51820;

      peers = lib.mapAttrsToList (hostname: hostcfg: {
        inherit (hostcfg) publicKey;
        allowedIPs = [ "${hostcfg.ip6}/128" ]
          ++ (hostcfg.routes.${config.networking.hostName} or []);
      } // (lib.optionalAttrs (builtins.length (endpointsOf6 hostcfg) > 0) {
        endpoint = "${builtins.head (endpointsOf6 hostcfg)}:${toString hostcfg.port or "51820"}";
        persistentKeepalive = 30;
      })) (lib.filterAttrs (hostname: _:
        peerable config.networking.hostName hostname
      ) peers);

      # Allow wireguard to route traffic to the internet
      postUp = ''
        ${iptables}/bin/ip6tables -A FORWARD -i wg0v6 -j ACCEPT
        ${iptables}/bin/ip6tables -t nat -A POSTROUTING -s ${currentPeer.ip6}/96 -o eno? -j MASQUERADE
      '';

      # Undo the above
      preDown = ''
        ${iptables}/bin/ip6tables -D FORWARD -i wg0v6 -j ACCEPT
        ${iptables}/bin/ip6tables -t nat -D POSTROUTING -s ${currentPeer.ip6}/96 -o eno? -j MASQUERADE
      '';
    };
  };

  systemd.services.wireguard-wg0.unitConfig.Before = [ "sshd.service" ];
  systemd.services.wireguard-wg1.unitConfig.Before = [ "sshd.service" ];

  secrets.files = {
    wireguard = {
      file = usr.secrets.keyDir + "/wireguard/${config.networking.hostName}.key";
     #user = "root";
     #group = "root";
    };
  };
}
