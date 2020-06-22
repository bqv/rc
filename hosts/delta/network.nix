{ config, lib, pkgs, hosts, inputs, ... }:

let
  iwdModule = "services/networking/iwd.nix";
in {
  disabledModules = [ iwdModule ];
  imports = [ "${inputs.pr75800}/nixos/modules/${iwdModule}" ];

  environment.systemPackages = with pkgs; [ dhcp dhcpcd ];

  networking.wireless = let
    useIwd = true;
  in {
    enable = !useIwd;
    iwd = {
      enable = useIwd;
      networks = lib.mkIf (useIwd)
        (lib.mapAttrs (k: v: { passphrase = v.psk; }) (import ../../secrets/wifi.networks.nix));
    };
    networks = lib.mkIf (!useIwd)
      (import ../../secrets/wifi.networks.nix);

    interfaces = [ "wlp3s0" ];
    userControlled.enable = true;
  };

  networking.useNetworkd = true;
  users.users.resolved.uid = 57; # unused: was network-manager
  networking.useDHCP = false;
  networking.enableIPv6 = true;
  networking.defaultGateway = hosts.lan.router;
  networking.nameservers = [ "9.9.9.9" "1.1.1.1" ];
  networking.interfaces.eno1 = {
    useDHCP = true;
    ipv4.addresses = [{ address = hosts.lan.delta-wired; prefixLength = 24; }];
  };
  networking.interfaces.wlp3s0 = {
    useDHCP = true;
    ipv4.addresses = [{ address = hosts.lan.delta-wireless; prefixLength = 24; }];
  };

  networking.interfaces.enp5s0u1 = {
    useDHCP = true;
    ipv4.addresses = [{ address = hosts.lan.delta-eth; prefixLength = 24; }];
  }; systemd.services.network-link-enp5s0u1.before = [];
  networking.interfaces.enp0s20u3u1u2 = {
    useDHCP = true;
  }; systemd.services.network-link-enp0s20u3u1u2.before = [];

  networking.firewall.enable = false;
  networking.nftables = {
    enable = true;
    ruleset = ''
      table inet filter {
        chain input {
          type filter hook input priority 0;

          # accept any localhost traffic
          iifname lo accept

          # accept traffic originated from us
          ct state {established, related} accept

          # ICMP
          # routers may also want: mld-listener-query, nd-router-solicit
          ip6 nexthdr icmpv6 icmpv6 type { destination-unreachable, packet-too-big, time-exceeded, parameter-problem, nd-router-advert, nd-neighbor-solicit, nd-neighbor-advert } accept
          ip protocol icmp icmp type { destination-unreachable, router-advertisement, time-exceeded, parameter-problem } accept

          # allow "ping"
          ip6 nexthdr icmpv6 icmpv6 type echo-request accept
          ip protocol icmp icmp type echo-request accept

          # accept SSH connections (required for a server)
          tcp dport 22 accept

          accept
        }

        chain output {
          type filter hook output priority 0;
          accept
        }

        chain forward {
          type filter hook forward priority 0;
          accept
        }
      }
    '';
  };

  assertions = [
    {
      assertion = with config; networking.useDHCP == false;
      message = ''
        The global useDHCP flag is deprecated. Per-interface useDHCP will be mandatory in the future.
      '';
    }
  ];
}
