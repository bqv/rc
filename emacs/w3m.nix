{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.w3m = {
    enabled = false;
    demand = true;
    config = ''
      (setq w3m-display-inline-images t)
    '';
  };
}
