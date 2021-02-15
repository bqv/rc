{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.vc-darcs = {
    demand = true;
    systemDeps = with pkgs; [ darcs ];
  };
}
