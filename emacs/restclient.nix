{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.restclient = {
    demand = true;
  };
}
