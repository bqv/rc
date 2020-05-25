{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.flycheck-purescript = {
    demand = true;
  };
}
