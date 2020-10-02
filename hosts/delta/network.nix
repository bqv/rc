{ config, lib, pkgs, hosts, flakes, usr, ... }:

{
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

  networking.nftables = let
    inherit (usr) dag;
  in {
    enable = true;

    rules = {
      inet.filter.input = {
        weechat = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "tcp"; field = "dport";
          value = [ 6667 6697 ];
          policy = "accept";
        };
        ipfs-api-tcp = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "tcp"; field = "dport";
          value = 4001;
          policy = "accept";
        };
        ipfs-api-udp = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "udp"; field = "dport";
          value = 4001;
          policy = "accept";
        };
        ipfs-gw-tcp = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "tcp"; field = "dport";
          value = 4501;
          policy = "accept";
        };
        ipfs-gw-udp = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "udp"; field = "dport";
          value = 4501;
          policy = "accept";
        };
        ipfs-cluster-tcp = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "tcp"; field = "dport";
          value = 9096;
          policy = "accept";
        };
        ipfs-cluster-udp = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "udp"; field = "dport";
          value = 9096;
          policy = "accept";
        };
        mpd = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "tcp"; field = "dport";
          value = 6600;
          policy = "accept";
        };
        theta = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["ssh" "default"] {
          protocol = "ip"; field = "saddr";
          value = hosts.lan.theta;
          policy = "accept";
        };
      };
    };
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
