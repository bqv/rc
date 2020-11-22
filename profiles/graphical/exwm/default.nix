{ config, lib, pkgs, ... }:

{
  services.xserver = {
    windowManager.session = lib.singleton {
      name = "exwm";
      start = ''
        ${pkgs.gccEmacs}/bin/emacs
      '';
    };
  };
}
