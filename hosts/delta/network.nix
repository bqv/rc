{ config, lib, pkgs, hosts, usr, ... }:

{
  boot.initrd = {
    availableKernelModules = [ "e1000e" ];
    network.enable = true;
    network.ssh.enable = true;
    network.ssh.hostKeys = map (f: f.path) config.services.openssh.hostKeys;
    network.ssh.authorizedKeys = [
      "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBILyD517o16vk3wOh2O4NNDJ0zoUOjaP4BSeonprVyurnw0HCuQ5T9rVhaDerI4Yndr85pSzqGU46LdzjibSwKA= ssh@theta"
    ];
  };
  environment.systemPackages = with pkgs; [ dhcp dhcpcd ];

  networking.namespacing.enable = false;
  networking.wireless = let
    useIwd = true;
  in {
    enable = !useIwd;
    iwd = {
      enable = useIwd;
      networks = lib.mkIf (useIwd)
        (lib.mapAttrs (k: v: { passphrase = v.psk; }) usr.secrets.wifi.networks);
    };
    networks = lib.mkIf (!useIwd) usr.secrets.wifi.networks;

    interfaces = [ "wlp0s20f3" ];
    userControlled.enable = true;
  };

 #networking.useNetworkd = true;
 #users.users.resolved.uid = 57; # unused: was network-manager
 #systemd.services.systemd-resolved.environment = {
 #  LD_LIBRARY_PATH = "${lib.getLib pkgs.libidn2}/lib";
 #};
  networking.useDHCP = false;
  networking.dhcpcd = {
    enable = true;
    persistent = true;
  };
  networking.enableIPv6 = true;
  networking.defaultGateway = { address = hosts.lan.router; interface = "enp0s31f6"; };
  networking.nameservers = [ "2a00:1098:2c::1" ];
  networking.interfaces.enp0s31f6 = {
 #  useDHCP = true;
    ipv4.addresses = [{ address = hosts.lan.delta-wired; prefixLength = 24; }];
    ipv6.addresses = [ hosts.ipv6.delta ];
  };
  networking.interfaces.wlp0s20f3 = {
 #  useDHCP = true;
    ipv4.addresses = [{ address = hosts.lan.delta-wireless; prefixLength = 24; }];
    ipv6.addresses = [ hosts.ipv6.delta ];
  };

  networking.interfaces.enp4s0u1 = {
 #  useDHCP = true;
    ipv4.addresses = [{ address = hosts.lan.delta-eth; prefixLength = 24; }];
    ipv6.addresses = [ hosts.ipv6.delta ];
  }; systemd.services.network-link-enp4s0u1.before = [];
  networking.interfaces.enp0s20u3u1u2 = {
 #  useDHCP = true;
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
          value = [ 4001 5001 ];
          policy = "accept";
        };
        ipfs-api-udp = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "udp"; field = "dport";
          value = [ 4001 5001 ];
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
        searx = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "tcp"; field = "dport";
          value = 8888;
          policy = "accept";
        };
        hydra = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "tcp"; field = "dport";
          value = 9999;
          policy = "accept";
        };
        udpports = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "udp"; field = "dport";
          value = map (x: x+32768) (lib.genList (x: x+1) (65535-32768));
          # mosh: 60000-65535
          # chromecast: 32768-61000
          policy = "accept";
        };
        ssdp = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["ssh" "default"] {
          protocol = "udp"; field = "dport";
          value = 1900;
          policy = "accept";
        };
        omega = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["ssh" "default"] {
          protocol = "ip"; field = "saddr";
          value = hosts.lan.omega;
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
 #systemd.services.systemd-networkd-wait-online.serviceConfig.ExecStart = lib.mkIf config.networking.useNetworkd [
 #  ""
 #  "${pkgs.coreutils}/bin/sleep 10"
 #];
}
