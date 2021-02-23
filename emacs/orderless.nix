{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.orderless = {
    demand = true;
    after = [ "ivy" ];
    config = ''
      (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
      ;(add-to-list 'completion-styles 'orderless-ivy-re-builder)
    '';
  };
}
