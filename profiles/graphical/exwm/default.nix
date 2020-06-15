{ config, lib, pkgs, ... }:

{
  services.xserver = {
    windowManager.session = lib.singleton {
      name = "exwm";
      start = ''
        ${pkgs.gccEmacs.override {
          withXwidgets = true;
          webkitgtk = pkgs.large.webkitgtk;
        }}/bin/emacs
      '';
    };
    displayManager = with pkgs; {
      defaultSession = "none+exwm";
    };
  };
}
