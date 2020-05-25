{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.tracking = {
    demand = true;
  };
}
