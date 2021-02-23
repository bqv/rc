{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.ivy-rich = {
    demand = true;
    after = [ "ivy" "counsel" ];
    config = ''
      (add-hook 'ivy-mode-hook 'ivy-rich-mode)
      (ivy-rich-mode ivy-mode)

      (setq ivy-rich-project-root-cache-mode t)
      (setq ivy-rich-path-style 'abbrev)
    '';
  };
}
