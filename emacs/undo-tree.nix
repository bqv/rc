{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.undo-tree = {
    demand = true;
    config = ''
      (global-undo-tree-mode t)
    '';
  };
}
