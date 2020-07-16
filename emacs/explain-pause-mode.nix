{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.explain-pause-mode = {
    demand = true;
    config = ''
      (explain-pause-mode)
    '';
  };
}
