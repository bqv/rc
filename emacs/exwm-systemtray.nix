{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.exwm-systemtray = {
    after = [ "exwm" ];
    package = epkgs: epkgs.exwm;
    config = ''
      (progn
        (exwm-systemtray-enable))
    '';
  };
}
