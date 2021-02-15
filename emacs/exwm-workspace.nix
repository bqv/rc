{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.exwm-workspace = {
    after = [ "exwm" ];
    package = epkgs: epkgs.exwm;
    config = ''
      (progn
        (setq exwm-workspace-number 4)
        (setq exwm-workspace-show-all-buffers t)
        (setq exwm-layout-show-all-buffers t)
        (setq exwm-workspace-minibuffer-position 'top)
        (defun bqv/exwm-minibuffer-advice (old-function &rest args)
          (cl-flet ((rawdisplay
                     (str)
                     (replace-regexp-in-string "\\.0$" "" str)))
               (if (ignore-errors
                     (string-equal
                      (rawdisplay (slot-value exwm--connection 'display))
                      (rawdisplay (frame-parameter (selected-frame) 'display))))
                   (apply old-function args)))))
      (advice-add 'exwm-workspace--on-minibuffer-setup
                  :around 'bqv/exwm-minibuffer-advice)
    '';
  };
}
