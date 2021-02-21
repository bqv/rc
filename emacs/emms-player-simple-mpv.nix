{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.emms-player-simple-mpv = {
    demand = true;
    after = [ "emms" ];
    config = ''
      nil
    '';
  };
}
