{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.palette = {
    demand = true;
    package = lib.const null;
  };
}
