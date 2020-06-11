{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.magit = {
    demand = true;
    bind = {
      "C-x g" = "magit-status";
      "C-x M-g" = "magit-dispatch-popup";
    };
    config = ''
      (setq magit-log-show-refname-after-summary t)
    '';
    systemDeps = with pkgs; [ git ];
  };
}
