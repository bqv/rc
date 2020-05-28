{ config, lib, pkgs, ... }:

{
  services.xserver = {
    windowManager.session = lib.singleton {
      name = "exwm";
      start = ''
        ${pkgs.emacs}/bin/emacs
      '';
    };
    displayManager = with pkgs; {
      defaultSession = "none+exwm";
    };
  };
}
