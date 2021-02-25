{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.w3m = {
    demand = true;
    package = _: pkgs.emacs.pkgs.w3m;
    config = ''
      (setq w3m-display-inline-images t)
    '';
  };
}
