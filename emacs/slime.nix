{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.slime = {
    demand = true;
    config = ''
      (setq slime-auto-connect 'always)
    '';
  };
}
