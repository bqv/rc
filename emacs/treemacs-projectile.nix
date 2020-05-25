{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.treemacs-projectile = {
    demand = true;
    after = [ "treemacs" "projectile" ];
  };
}
