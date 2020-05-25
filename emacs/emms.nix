{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.emms = {
    demand = true;
    config = ''
      (emms-minimalistic) ;(emms-all)
      (emms-default-players)
    '';
  };
}
