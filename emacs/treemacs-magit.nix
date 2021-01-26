{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.treemacs-magit = {
    demand = true;
    after = [ "treemacs" "magit" ];
  };
}
