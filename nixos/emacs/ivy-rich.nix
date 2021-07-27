{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.ivy-rich = {
    demand = true;
    after = [ "ivy" "counsel" ];
    config = ''
      (defvar ivy-mode nil)
      (defvar counsel-mode nil)
      (defvar counsel-projectile-mode nil)

      (defun maybe-restart-counsel-projectile ()
        (when counsel-projectile-mode
          (counsel-projectile-mode nil))
        (counsel-projectile-mode t))

      (add-hook 'ivy-mode-hook 'ivy-rich-mode)
      (add-hook 'ivy-rich-mode-hook 'maybe-restart-counsel-projectile)
      (ivy-rich-mode ivy-mode)

      (setq ivy-rich-project-root-cache-mode t)
      (setq ivy-rich-path-style 'abbrev)
    '';
  };
  emacs.loader.all-the-icons-ivy = {
    demand = true;
    after = [ "ivy" "all-the-icons" ];
  };
  emacs.loader.all-the-icons-ivy-rich = {
    demand = true;
    after = [ "ivy-rich" "all-the-icons" ];
    config = ''
      (add-hook 'ivy-mode-hook 'all-the-icons-ivy-rich-mode)
      (add-hook 'all-the-icons-ivy-rich-mode-hook 'maybe-restart-counsel-projectile)
      (all-the-icons-ivy-rich-mode ivy-mode)
    '';
  };
}
