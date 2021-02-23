{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.ivy-rich = {
    demand = true;
    after = [ "ivy" "counsel" ];
    config = ''
      (setq ivy-rich-project-root-cache-mode t)
      (add-hook 'ivy-mode-hook 'ivy-rich-mode)
      (setq ivy-rich-path-style 'abbrev)
      (ivy-rich-mode 1)
    '';
  };
}
