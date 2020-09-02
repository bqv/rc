{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.eshell-z = {
    demand = true;
  };
}
