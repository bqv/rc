{ config, lib, usr, pkgs, domains, ... }:

{
  emacs.loader.bookmark = {
    demand = true;
    package = lib.const null;
    config = ''
      (defun bookmark-save-advice (&rest r)
        (let ((save-silently t))
          (bookmark-save)))
      (advice-add 'bookmark-set :after #'bookmark-save-advice)
    '';
  };
}
