{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.s = {
    demand = true;
  };
}
