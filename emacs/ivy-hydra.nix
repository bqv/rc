{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.ivy-hydra = {
    demand = true;
    after = [ "ivy" ];
  };
}
