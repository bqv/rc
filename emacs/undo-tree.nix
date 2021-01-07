{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.undo-tree = {
    demand = true;
  };
}
