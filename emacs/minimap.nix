{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.minimap = {
    demand = true;
  };
}
