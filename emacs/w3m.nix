{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.w3m = {
    enable = false;
    demand = true;
    config = ''
      (setq w3m-display-inline-images t)
    '';
  };
}
