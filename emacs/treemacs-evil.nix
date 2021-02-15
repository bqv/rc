{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.treemacs-evil = {
    demand = true;
    after = [ "treemacs" "evil" ];
  };
}
