{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.md4rd = {
    demand = true;
    config = ''
      (defalias #'reddit-mode #'md4rd)
    '';
  };
}
