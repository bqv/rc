{ config, lib, pkgs, ... }:

{
  services.xserver = {
    windowManager.session = lib.singleton {
      name = "exwm";
      start = ''
        ${pkgs.emacsPgtkGcc}/bin/emacs
      '';
    };
  };
}
