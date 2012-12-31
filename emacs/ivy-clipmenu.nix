{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.ivy-clipmenu = {
    demand = true;
    after = [ "ivy" ];
  };
}
