{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.which-key = {
    demand = true;
    diminish = [ "which-key-mode" ];
    config = ''
      (add-hook 'after-init-hook 'which-key-mode)
    '';
  };
}
