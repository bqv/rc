{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.ace-window = {
    enable = false;
    demand = true;
    bind = {
      "M-m" = "ace-window";
    };
    config = ''
      (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
            aw-dispatch-always t)
    '';
  };
}
