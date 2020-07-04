{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.darcsum = {
    demand = true;
    systemDeps = with pkgs; [ darcs ];
  };
}
