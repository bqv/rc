{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.rcirc-color = {
    demand = true;
    after = [ "rcirc" ];
  };
}
