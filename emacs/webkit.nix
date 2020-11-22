{ config, lib, usr, pkgs, domains, ... }:

{
  emacs-loader.webkit = {
    demand = true;
    package = epkgs: epkgs.emacs-webkit;
    bind = {
      "M-*" = "webkit";
    };
    config = ''
      (with-eval-after-load 'evil
        (require 'evil-collection-webkit)
        (evil-collection-xwidget-setup))
      (setq webkit-own-window nil)
      (setq webkit-search-prefix "https://qwant.com/?q=")
    '';
  };
}
