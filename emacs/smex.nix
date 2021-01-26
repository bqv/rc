{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.smex = {
    demand = true;
    config = ''
      (smex-initialize)
      (global-set-key (kbd "C-c C-c M-x") 'smex)
      (global-set-key (kbd "C-M-x") 'smex-major-mode-commands)
    '';
  };
}
