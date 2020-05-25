{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.company-cabal = {
    demand = true;
  };
}
