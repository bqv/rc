{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.ace-window = {
    enable = false;
    demand = true;
    bind = {
      "M-m" = "ace-window"; # deprecating
      "s-m" = "ace-window"; # usurped from switch-window
    };
    config = ''
      (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
            aw-dispatch-always t
            aw-scope 'global
            aw-minibuffer-flag t)
      (ace-window-display-mode)
      (exwm-input-set-key (kbd "s-m") 'ace-window)
      (exwm-input--update-global-prefix-keys)
    '';
  };
}
