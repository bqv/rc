{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.leaf-convert = {
    demand = true;
  };
}
