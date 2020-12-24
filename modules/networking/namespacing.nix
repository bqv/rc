{ config, lib, pkgs, ... }:

let
  cfg = config.networking.namespacing;

  makeServiceNsPhysical = name: {
    systemd.services."${name}".serviceConfig.NetworkNamespacePath = "/var/run/netns/physical";
  };
  makeSocketNsPhysical = name: {
    systemd.sockets."${name}".socketConfig.NetworkNamespacePath = "/var/run/netns/physical";
  };
  unRestrictNamespaces = name: {
    systemd.sockets."${name}".socketConfig.RestrictNamespaces = "~net";
  };
in {
  options = with lib; {
    networking.namespacing = {
      enable = mkEnableOption "full system network namespacing";
      virtualSplit = mkEnableOption "virtual namespace by default" // {
        default = true;
      };
    };
  };

  config = lib.mkIf cfg.enable ((lib.mkMerge [
    # networkmanager units:
    (makeServiceNsPhysical "NetworkManager")
    (makeServiceNsPhysical "NetworkManager-dispatcher")
    (makeServiceNsPhysical "NetworkManager-wait-online")
    (makeServiceNsPhysical "ModemManager")

    # without networkmanager:
    (makeServiceNsPhysical "dhcpcd")
    (makeServiceNsPhysical "systemd-networkd")
    (makeServiceNsPhysical "systemd-resolved")
    (unRestrictNamespaces "systemd-resolved")
    (makeServiceNsPhysical "systemd-timesyncd")
    (makeServiceNsPhysical "systemd-networkd-wait-online")
    (makeServiceNsPhysical "network-local-commands")
    (makeServiceNsPhysical "nftables")
    (makeServiceNsPhysical "nix-daemon")
    (makeServiceNsPhysical "nscd")
    (makeServiceNsPhysical "weechat")
    (makeServiceNsPhysical "ipfs")
    (makeServiceNsPhysical "wireguard-wg0")
    (makeSocketNsPhysical "sshd")
  ]) // {
    boot = {
      loader.timeout = lib.mkForce 2;

      systemdExecutable = toString (
        pkgs.writeShellScript "systemd-shim" ''set -eux
          ${pkgs.iproute}/bin/ip netns add virtual
          ${pkgs.coreutils}/bin/touch /var/run/netns/physical
          ${pkgs.utillinux}/bin/mount -o bind /proc/self/ns/net /var/run/netns/physical
          exec ${pkgs.iproute}/bin/ip netns exec ${if cfg.virtualSplit then "virtual" else "physical"} systemd
        ''
      );
    };

    systemd.services.systemd-networkd.serviceConfig.ExecStartPre = pkgs.writeScript "setup-virtual-ns" ''
      ${pkgs.iproute}/bin/ip link add v0 type veth peer name p0

      # configure the "p0" device, which is
      # the link TO p0, and lives in "physical"
      ${pkgs.iproute}/bin/ip addr add 169.254.1.1/24 dev p0
      ${pkgs.iproute}/bin/ip link set dev p0 netns physical
      ${pkgs.iproute}/bin/ip netns exec physical ip link set p0 up

      ${pkgs.iproute}/bin/ip netns exec physical bash -c 'echo 0 > /proc/sys/net/ipv4/conf/all/forwarding'
      ${pkgs.iproute}/bin/ip netns exec physical bash -c 'echo 1 > /proc/sys/net/ipv4/conf/eno1/forwarding
      ${pkgs.iproute}/bin/ip netns exec physical bash -c 'echo 1 > /proc/sys/net/ipv4/ip_forward'
      ${pkgs.iproute}/bin/ip netns exec physical iptables -t nat -A POSTROUTING -o eno1 -j MASQUERADE
      ${pkgs.iproute}/bin/ip netns exec physical iptables -I FORWARD -i eno1 -o p0 -m state --state RELATED,ESTABLISHED -j ACCEPT
      ${pkgs.iproute}/bin/ip netns exec physical iptables -I FORWARD -i p0 -o eno1 -j ACCEPT

      # configure the "v0" device, which is
      # the link TO v0, and lives in "virtual"
      ${pkgs.iproute}/bin/ip addr add 169.254.1.2/24 dev v0
      ${pkgs.iproute}/bin/ip link set dev v0 netns virtual
      ${pkgs.iproute}/bin/ip netns exec virtual ip route add default via 169.254.1.1 dev v0
      ${pkgs.iproute}/bin/ip netns exec virtual ip link set v0 up
    '';
  });
}
