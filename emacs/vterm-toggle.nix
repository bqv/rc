{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.vterm-toggle = {
    demand = true;
    config = ''
      (setq vterm-toggle-reset-window-configration-after-exit nil)
    '';
  };
}
