{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.general = {
    demand = true;
  };
}
