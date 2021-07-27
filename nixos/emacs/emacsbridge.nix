{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.emacsbridge = {
    demand = true;
    require = [ "emacsbridge-rpc" "alert-emacsbridge" ];
    config = ''
    '';
  };
}
