{ config, lib, pkgs, hosts, inputs, ... }:

let
  iwdModule = "services/networking/iwd.nix";
in {
  disabledModules = [ iwdModule ];
  imports = [ "${inputs.pr75800}/nixos/modules/${iwdModule}" ];

  environment.systemPackages = with pkgs; [ dhcp dhcpcd ];

  networking.namespacing.enable = false;
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

    interfaces = [ "wlp0s20f3" ];
    userControlled.enable = true;
  };

  networking.useNetworkd = true;
  users.users.resolved.uid = 57; # unused: was network-manager
  networking.useDHCP = false;
  networking.enableIPv6 = true;
  networking.defaultGateway = hosts.lan.router;
  networking.nameservers = [ "9.9.9.9" "1.1.1.1" ];
  networking.interfaces.enp0s31f6 = {
    useDHCP = true;
    ipv4.addresses = [{ address = hosts.lan.delta-wired; prefixLength = 24; }];
  };
  networking.interfaces.wlp0s20f3 = {
    useDHCP = true;
    ipv4.addresses = [{ address = hosts.lan.delta-wireless; prefixLength = 24; }];
  };

  networking.interfaces.enp4s0u1 = {
    useDHCP = true;
    ipv4.addresses = [{ address = hosts.lan.delta-eth; prefixLength = 24; }];
  }; systemd.services.network-link-enp4s0u1.before = [];
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

          #tcp dport 22 accept # SSH connections

          tcp dport 6697 accept # Weechat

          log
          drop
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

  # Disable `systemd-networkd-wait-online` - it's just too buggy
  systemd.services.systemd-networkd-wait-online.serviceConfig.ExecStart = lib.mkIf config.networking.useNetworkd [
    ""
    "${pkgs.coreutils}/bin/sleep 10"
  ];
}
