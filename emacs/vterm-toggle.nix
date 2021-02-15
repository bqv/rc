{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.vterm-toggle = {
    demand = true;
  };
}
