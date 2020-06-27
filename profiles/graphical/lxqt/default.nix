{ config, lib, pkgs, ... }:

{
  services.xserver = {
    desktopManager = {
      lxqt.enable = true;
    };
  };
}
