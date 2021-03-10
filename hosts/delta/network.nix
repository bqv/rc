{ config, lib, pkgs, hosts, usr, ... }:

{
  boot.initrd = {
    availableKernelModules = [ "xhci_hcd" ];
    network.enable = true;
    network.flushBeforeStage2 = false;
    network.ssh.enable = true;
    network.ssh.hostKeys = map (f: f.path) config.services.openssh.hostKeys;
    network.ssh.authorizedKeys = [
      "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBILyD517o16vk3wOh2O4NNDJ0zoUOjaP4BSeonprVyurnw0HCuQ5T9rVhaDerI4Yndr85pSzqGU46LdzjibSwKA= ssh@theta"
    ];
  };

  environment.systemPackages = with pkgs; [ dhcp dhcpcd mactelnet ];

  networking.namespacing.enable = false;
  networking.wlanInterfaces = lib.mapAttrs (_: x: {
    device = "wlan0";
  } // x) {
    wlan0 = { };
    adhoc0 = {
      type = "ibss";
    };
    p2p0 = { };
  };
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

    interfaces = [ "wlan0" ];
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
  networking.defaultGateway = { address = hosts.lan.router; };
  networking.defaultGateway6 = { address = "${hosts.ipv6.home.prefix}:1"; };
  networking.nameservers = [ "2a00:1098:2c::1" ];

  networking.interfaces = rec {
    lan0 = {
      useDHCP = true;
      ipv4.addresses = [{ address = hosts.lan.delta-wired; prefixLength = 24; }];
      ipv6.addresses = [ hosts.ipv6.delta-wired ];
    }; enp0s31f6 = lan0; eno2 = lan0;
    wlan0 = {
      useDHCP = true;
      ipv4.addresses = [{ address = hosts.lan.delta-wireless; prefixLength = 32; }];
      ipv6.addresses = [ hosts.ipv6.delta-wireless ];
    }; wlp3s0 = wlan0;

    enp4s0u1 = {
      useDHCP = false;
      ipv4.addresses = [{ address = hosts.lan.delta-eth; prefixLength = 32; }];
      ipv6.addresses = [ hosts.ipv6.delta-eth ];
    };
    enp0s20u3u1u2 = {
      useDHCP = true;
      ipv6.addresses = [ hosts.ipv6.usb-eth ];
    };
  };
  systemd.services.network-link-enp4s0u1.before = [];
  systemd.services.network-link-enp0s20u3u1u2.before = [];

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
          value = lib.range 32768 65535;
          # chromecast: 32768-61000
          # mosh: 60000-65535
          policy = "accept";
        };
        multicast = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "meta"; field = "pkttype";
          value = [ "broadcast" "multicast" ];
          policy = "accept";
        };
        mactelnet = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "udp"; field = "dport";
          value = 20561;
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

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ACTION=="add", ATTR{address}=="b8:ae:ed:7b:d9:e3", NAME="lan0"
    SUBSYSTEM=="net", ACTION=="add", ATTR{address}=="00:21:5c:b7:6c:72", NAME="wlan0"
  '';

 ## Disable `systemd-networkd-wait-online` - it's just too buggy
 #systemd.services.systemd-networkd-wait-online.serviceConfig.ExecStart = lib.mkIf config.networking.useNetworkd [
 #  ""
 #  "${pkgs.coreutils}/bin/sleep 10"
 #];

  systemd.services.mactelnet = {
    description = "MacTelnet Server";
    environment.PATH = lib.mkForce "/run/wrappers/bin:/nix/var/nix/profiles/default/bin:/run/current-system/sw/bin";
    serviceConfig.ExecStart = "${pkgs.mactelnet}/bin/mactelnetd -fn";
    wantedBy = [ "multi-user.target" ];
  };
  environment.etc."mactelnetd.users".text = ''
    root:${config.users.users.root.password}
  '';
}
