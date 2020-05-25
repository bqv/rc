{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.direnv = {
    demand = true;
    config = ''
      (direnv-mode)
    '';
  };
}
