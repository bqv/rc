{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.tramp = {
    demand = true;
    package = lib.const null;
    config = ''
      (require 'tramp)
      ;(add-to-list 'tramp-remote-path "/run/current-system/sw/bin")
      (add-to-list 'tramp-remote-path "/run/wrappers/bin")
      (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
    '';
  };
}
