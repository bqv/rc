{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.slime = {
    demand = true;
  };
}
