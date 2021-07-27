{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.disk-usage = {
    demand = true;
  };
}
