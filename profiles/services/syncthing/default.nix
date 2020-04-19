{ config, pkgs, ... }:

{
  services.syncthing.enable = true;
  services.syncthing.openDefaultPorts = true;
}
