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
        mosh = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "udp"; field = "dport";
          value = [ 60000 60001 60002 60003 60004 60005 60006 60007 60008 60009
                    60010 60011 60012 60013 60014 60015 60016 60017 60018 60019
                    60020 60021 60022 60023 60024 60025 60026 60027 60028 60029
                    60030 60031 60032 60033 60034 60035 60036 60037 60038 60039
                    60040 60041 60042 60043 60044 60045 60046 60047 60048 60049
                    60050 60051 60052 60053 60054 60055 60056 60057 60058 60059
                    60060 60061 60062 60063 60064 60065 60066 60067 60068 60069
                    60070 60071 60072 60073 60074 60075 60076 60077 60078 60079
                    60080 60081 60082 60083 60084 60085 60086 60087 60088 60089
                    60090 60091 60092 60093 60094 60095 60096 60097 60098 60099 ];
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
