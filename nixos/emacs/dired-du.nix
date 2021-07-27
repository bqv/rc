{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.dired-du = {
    demand = true;
  };
}
