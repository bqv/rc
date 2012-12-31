{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.auctex-lua = {
    demand = true;
    after = [ "auctex" ];
  };
}
