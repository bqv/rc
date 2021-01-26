{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.go-mode = {
    demand = true;
    config = ''
      (add-hook 'go-mode-hook (lambda () (setq tab-width 2)))
    '';
  };
}
