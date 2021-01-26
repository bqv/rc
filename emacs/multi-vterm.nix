{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.multi-vterm = {
    demand = true;
    after = [ "vterm" ];
    config = ''
      (define-key vterm-mode-map (kbd "C-x <tab>") #'multi-vterm-next)
      (define-key vterm-mode-map (kbd "C-x t") #'multi-vterm)
    '';
  };
}
