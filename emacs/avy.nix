{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.avy = {
    demand = true;
    bind = {
      "C-=" = "avy-goto-char";
    };
    config = ''
      (setq avy-background t)
    '';
  };
}
