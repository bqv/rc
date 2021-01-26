{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.execline = {
    demand = true;
  };
}
