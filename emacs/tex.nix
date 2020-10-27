{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.tex = {
    demand = true;
    package = lib.const null;
  };
}
