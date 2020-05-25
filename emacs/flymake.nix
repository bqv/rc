{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.flymake = {
    demand = true;
  };
}
