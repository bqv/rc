{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.nix-mode = {
    demand = true;
  };
}
