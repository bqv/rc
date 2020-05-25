{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.racket-mode = {
    demand = true;
    config = ''
      (setq scheme-program-name "racket")
    '';
  };
}
