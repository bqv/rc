{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.calfw = {
    demand = true;
  };
}
