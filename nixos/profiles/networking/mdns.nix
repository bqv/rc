{ config, pkgs, ... }:

{
  services.avahi.enable = true;
  services.avahi.nssmdns = true;
  services.avahi.reflector = true;
  services.avahi.allowPointToPoint = true;
  services.avahi.ipv4 = true;
  services.avahi.ipv6 = true;
  services.avahi.publish.enable = true;
  services.avahi.publish.addresses = true;
}
