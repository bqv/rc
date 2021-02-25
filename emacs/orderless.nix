{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.orderless = {
    demand = true;
    after = [ "ivy" ];
  };
}
