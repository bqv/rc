{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.shackle = {
    demand = true;
  };
}
