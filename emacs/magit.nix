{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.magit = {
    demand = true;
    bind = {
      "C-x g" = "magit-status";
    };
    config = ''
      (setq magit-log-show-refname-after-summary t)
    '';
  };
}
