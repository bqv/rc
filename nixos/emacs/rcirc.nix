{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.rcirc = {
    demand = true;
    package = lib.const null;
  };
}
