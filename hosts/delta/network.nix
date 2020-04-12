{ config, lib, pkgs, ... }:

{
  networking.wireless = {
    enable = true;
    interfaces = [ "wlp3s0" ];
    #iwd.enable = true; # pending https://github.com/NixOS/nixpkgs/pull/75800
    networks = import ../../secrets/wifi.networks.nix;
    userControlled.enable = true;
  };

  networking.useNetworkd = true;
  networking.useDHCP = false;
  networking.enableIPv6 = true;
  networking.defaultGateway = "192.168.0.1";
  networking.nameservers = [ "9.9.9.9" ];
  networking.interfaces.eno1 = {
    useDHCP = true;
    ipv4.addresses = [{ address = "192.168.0.254"; prefixLength = 24; }];
  };
  networking.interfaces.wlp3s0 = {
    useDHCP = true;
    ipv4.addresses = [{ address = "192.168.0.253"; prefixLength = 24; }];
  };

  networking.interfaces.enp5s0u1 = {
    useDHCP = true;
    ipv4.addresses = [{ address = "192.168.0.252"; prefixLength = 24; }];
  }; systemd.services.network-link-enp5s0u1.before = [];
  networking.interfaces.enp0s20u3u1u2 = {
    useDHCP = true;
  }; systemd.services.network-link-enp0s20u3u1u2.before = [];

  networking.firewall.enable = false;

  assertions = [
    {
      assertion = with config; networking.useDHCP == false;
      message = ''
        The global useDHCP flag is deprecated. Per-interface useDHCP will be mandatory in the future.
      '';
    }
  ];
}
