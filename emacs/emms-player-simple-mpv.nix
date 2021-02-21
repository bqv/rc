{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.emms-player-simple-mpv = {
    demand = true;
    after = [ "emms" ];
    config = ''
      (push 'emms-player-mpd emms-player-list)
    '';
  };
}
