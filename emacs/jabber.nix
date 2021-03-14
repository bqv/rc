{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.jabber = {
    demand = true;
    config = ''
      nil
    '';
  };
}
