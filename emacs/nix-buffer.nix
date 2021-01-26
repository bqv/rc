{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.nix-buffer = {
    demand = true;
  };
}
