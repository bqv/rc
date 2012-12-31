{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.solarized-theme = {
    demand = true;
  };
}
