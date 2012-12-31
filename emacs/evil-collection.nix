{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.evil-collection = {
    demand = true;
  };
}
