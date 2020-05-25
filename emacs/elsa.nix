{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.elsa = {
    demand = true;
  };
}
