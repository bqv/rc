{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.matrix-client = {
    demand = true;
  };
}
