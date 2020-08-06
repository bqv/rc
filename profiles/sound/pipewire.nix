{ config, lib, pkgs, ... }:

{
  services.pipewire = {
    enable = true;
  };

  xdg.portal = {
    enable = true;
    gtkUsePortal = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
    ];
  };
}
