{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.gnus = {
    demand = true;
    package = lib.const null;
    config = ''
      nil
    '';
  };
}
