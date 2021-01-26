{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.exwm-xim = {
    demand = true;
    after = [ "exwm" ];
    package = epkgs: epkgs.exwm;
    config = ''
      (exwm-xim-enable)
    '';
  };
}
