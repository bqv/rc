{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.helm = {
    demand = true;
    config = ''
      nil
    '';
  };
}
