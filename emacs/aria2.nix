{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.aria2 = {
    demand = true;
  };
}
