{ config, pkgs, lib, usr, hosts, ... }:

let
  cfg = config.networking.wireguard;

  pubkeys = usr.secrets.wireguard.pubkeys;
  iptables = pkgs.iptables-nftables-compat;

  network = 16;#24;
  network6 = 112;

  currentPeer = cfg.peers."${config.networking.hostName}";
  isLan = peer: builtins.length (cfg.peers.${peer}.localArea or []) > 0;
  isLan6 = peer: builtins.length (cfg.peers.${peer}.localArea6 or []) > 0;
  isWan = peer: !(isLan peer);
  isWan6 = peer: !(isLan6 peer);
  endpointsOf = peer: (peer.wideArea or []) ++ (peer.localArea or []);
  endpointsOf6 = peer: (peer.wideArea6 or []) ++ (peer.localArea6 or []);
  peerable = from: to: builtins.all lib.id [
    (from != to)
    (cfg.peers.${to} ? publicKey)
  ];
in {
  options = {
    networking.wireguard = {
      peers = lib.mkOption {
        type = with lib.types; attrsOf (attrsOf (attrsOf anything));
        default = {
          # Note: Wireguard won't retry DNS resolution if it fails
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
          delta = {
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
          phi = {
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
        };
      };
    };
  };
  config = {
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
        ) cfg.peers);

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
        ) cfg.peers);

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
  };
}
