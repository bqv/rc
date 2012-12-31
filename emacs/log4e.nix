{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.log4e = {
    demand = true;
  };
}
