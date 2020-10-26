{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.bufler = {
    demand = true;
    config = ''
      (delq "VC" bufler-columns)
    '';
  };
}
