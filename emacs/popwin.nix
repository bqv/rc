{ config, lib, usr, pkgs, hosts, ... }:

{
  emacs.loader.popwin = {
    demand = true;
    config = ''
      (global-set-key (kbd "C-c w") popwin:keymap)
    '';
  };
}
