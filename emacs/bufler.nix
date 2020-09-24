{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.bufler = {
    demand = true;
  };
}
