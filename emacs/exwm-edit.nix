{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.exwm-edit = {
    demand = true;
    after = [ "exwm" ];
  };
}
