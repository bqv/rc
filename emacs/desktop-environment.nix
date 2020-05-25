{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.desktop-environment = {
    demand = true;
    after = [ "exwm-input" ];
    config = ''
      (progn
        (desktop-environment-mode))
    '';
  };
}
