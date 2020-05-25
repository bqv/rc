{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.treemacs-icons-dired = {
    demand = true;
    after = [ "treemacs" ];
  };
}
