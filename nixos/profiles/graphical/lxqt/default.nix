{ config, lib, pkgs, ... }:

{
  environment.systemPackages = [ pkgs.lxappearance ];

  services.xserver = {
    desktopManager = {
      lxqt.enable = true;
    };
  };
}
