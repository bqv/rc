{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.ivy = {
    demand = true;
    diminish = [ "ivy-mode" ];
    config = ''
      (defun ivy-rich-switch-buffer-transformer (arg)
        ; Compatibility hack
        arg)
      (setq-default ivy-initial-inputs-alist nil)
      (ivy-mode t)
      (add-hook 'after-init-hook
                (lambda (&rest r)
                  (setq ivy-initial-inputs-alist nil)))
    '';
  };
}
