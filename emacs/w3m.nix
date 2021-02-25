{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.w3m = {
    demand = true;
    require = []; # more gentle disable
    config = ''
      (setq w3m-display-inline-images t)
    '';
  };
}
