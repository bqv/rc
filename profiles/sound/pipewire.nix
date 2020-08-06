{ config, lib, pkgs, ... }:

{
  services.pulseaudio = lib.mkForce false;

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
