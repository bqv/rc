{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.emms = {
    demand = true;
    config = ''
      (require 'emms-setup nil t)
      (emms-minimalistic) ;(emms-all)
      (emms-default-players)
    '';
  };
}
