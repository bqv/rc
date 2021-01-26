{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.treemacs = {
    demand = true;
    bind = {
      "<f9>" = "treemacs";
    };
    config = ''
      (require 'electric)
      (defun electric-indent-local-mode (&rest r) nil)
    '';
  };
}
