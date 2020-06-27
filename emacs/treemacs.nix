{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.treemacs = {
    demand = true;
    config = ''
      (require 'electric)
      (defun electric-indent-local-mode (&rest r) nil)
    '';
  };
}
