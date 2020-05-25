{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.auctex-latexmk = {
    demand = true;
    after = [ "auctex" ];
  };
}
