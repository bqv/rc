{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.emms = {
    demand = true;
    require = [ "emms" "emms-player-mpd" ];
    config = ''
      (require 'emms-setup nil t)
      (emms-minimalistic) ;(emms-all)
      (emms-default-players)
      (emms-player-mpd-connect)
      (push 'emms-player-mpd emms-player-list)
    '';
  };
}
