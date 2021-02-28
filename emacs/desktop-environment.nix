{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.desktop-environment = {
    demand = true;
    after = [ "exwm-input" ];
    config = ''
      (setq desktop-environment-volume-get-command "amixer -c0 get Master")
      (setq desktop-environment-volume-set-command "amixer -c0 set Master %s")
      (setq desktop-environment-volume-toggle-command "amixer -c0 set Master toggle")
      (define-key desktop-environment-mode-map (kbd "<269025043>") #'desktop-environment-volume-increment) ; mouse v-up
      (define-key desktop-environment-mode-map (kbd "S-<269025043>") #'desktop-environment-volume-increment-slowly)
      (define-key desktop-environment-mode-map (kbd "<269025041>") #'desktop-environment-volume-decrement) ; mouse v-down
      (define-key desktop-environment-mode-map (kbd "S-<269025041>") #'desktop-environment-volume-decrement-slowly)
      (define-key desktop-environment-mode-map (kbd "<269025073>") #'emms-pause) ; headset btn

      (progn
        (desktop-environment-mode))
    '';
  };
}
