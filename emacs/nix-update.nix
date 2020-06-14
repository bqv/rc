{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.nix-update = {
    demand = true;
    after = [ "nix-mode" ];
    systemDeps = with pkgs; [ nix-prefetch-git ];
  };
}
