{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.exwm-config = {
    after = [ "exwm" ];
    package = epkgs: epkgs.exwm;
  };
}
