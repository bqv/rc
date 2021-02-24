{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.vterm-toggle = {
    enable = false;
    demand = true;
  };
}
