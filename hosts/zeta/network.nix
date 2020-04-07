{ config, pkgs, ... }:

let
  wanInterface = "eno1";
  v6Block = rec {
    addr = "${block}1";
    block = "2001:bc8:3de4::";
    subnet = "${block}/${toString prefix}";
    duid = "00:03:00:01:55:a4:d9:88:19:43";
    prefix = 48;
  };
  v6Subnets = {
    "2001:bc8:3de4:800::1" = {
      duid = "00:03:00:01:70:b7:ec:21:44:79";
      prefix = 56;
    };
    "2001:bc8:3de4:8cb::1" = {
      duid = "00:03:00:01:3e:bf:b2:66:2d:8e";
      prefix = 64;
    };
  };
in {
  networking.interfaces.${wanInterface} = {
    ipv4.addresses = [
      { address = "163.172.7.233"; prefixLength = 24; }
      { address = "195.154.56.65"; prefixLength = 24; }
    ];
    ipv6.addresses = let
      morph = block: { address = block.addr; prefixLength = block.prefix; };
      transform = addr: { address = addr; prefixLength = v6Subnets.${addr}.prefix; };
    in
      [ (morph v6Block) ] ++ map transform (builtins.attrNames v6Subnets);
    ipv6.routes = [
      { address = "2001:bc8:2::1:142:1"; prefixLength = 128; }
    ];
  };

  networking.defaultGateway = "163.172.7.1";
  networking.nameservers = [ "8.8.8.8" ];
  networking.hostName = "zeta";
  networking.nat.enable = true;
  networking.nat.internalInterfaces = ["ve-+"];
  networking.nat.externalInterface = wanInterface;

  environment.etc.dhclient6 = {
    target = "dhcp/dhclient6.conf";
    text = ''
      interface "${wanInterface}" {
         send dhcp6.client-id ${v6Block.duid};
      }
    '';
  };

  networking.enableIPv6 = true;
  systemd.services.dhclient = {
    description = "Client for sending IPv6 DUID";
    wants = [ "network-link-${wanInterface}.service" ];
    after = [ "network-link-${wanInterface}.service" ];
    before = [ "network-addresses-${wanInterface}.service" ];
    serviceConfig = {
      Type = "forking";
    };
    script = with config.environment; "${pkgs.dhcp}/sbin/dhclient -cf /etc/${etc.dhclient6.target} -6 -P -v ${wanInterface}";
    wantedBy = [ "network.target" ];
  };
  systemd.services."network-addresses-${wanInterface}" = {
    after = [ "dhclient.service" ];
    partOf = [ "dhclient.service" ];
  };
}
