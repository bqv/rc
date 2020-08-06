{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.web-mode = {
    demand = true;
  };
}
