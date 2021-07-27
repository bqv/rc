{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.flycheck-rust = {
    demand = true;
    config = ''
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    '';
  };
}
