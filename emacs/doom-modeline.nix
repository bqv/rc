{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.doom-modeline = {
    demand = true;
    after = [ "doom-themes" ];
  };
}
