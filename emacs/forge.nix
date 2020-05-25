{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.forge = {
    demand = true;
    after = [ "magit" ];
  };
}
