{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.counsel = {
    demand = true;
    after = [ "ivy" ];
    bind = {
      "M-x" = "counsel-M-x";
      "C-x M-f" = "counsel-recentf";
      #"C-y" = "counsel-yank-pop";
    };
    config = ''
      (setq ivy-initial-inputs-alist nil)
    '';
  };
}
