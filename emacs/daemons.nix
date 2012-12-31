{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.daemons = {
    demand = true;
    config = ''
      (setq daemons-always-sudo t)
    '';
  };
}
