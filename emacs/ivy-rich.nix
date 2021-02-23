{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.ivy-rich = {
    demand = true;
    after = [ "ivy" "counsel" ];
    config = ''
      (defvar ivy-mode nil)
      (defvar counsel-mode nil)
      (defvar counsel-projectile-mode nil)

      (add-hook 'ivy-mode-hook 'ivy-rich-mode)
      (ivy-rich-mode ivy-mode)

      (when counsel-projectile-mode
        (counsel-projectile-mode nil))
      (counsel-projectile-mode t)

      (setq ivy-rich-project-root-cache-mode t)
      (setq ivy-rich-path-style 'abbrev)
    '';
  };
  emacs.loader.all-the-icons-ivy = {
    demand = true;
    after = [ "ivy" "counsel" "all-the-icons" ];
  };
  emacs.loader.all-the-icons-ivy-rich = {
    demand = true;
    after = [ "ivy" "counsel" "all-the-icons" ];
    config = ''
      (add-hook 'ivy-mode-hook 'all-the-icons-ivy-rich-mode)
      (all-the-icons-ivy-rich-mode ivy-mode)
    '';
  };
}
