{ config, pkgs, ... }:

{
  services.syncthing.enable = true;
  services.syncthing.openDefaultPorts = true;
 #services.syncthing.declarative = {
 #  folders = {
 #  };
 #  devices = {
 #  };
 #};
}
