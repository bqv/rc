{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.all-the-icons = {
    demand = true;
    systemDeps = with pkgs; [ emacs-all-the-icons-fonts ];
  };
}
