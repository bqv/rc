{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.terraform-mode = {
    demand = true;
    systemDeps = with pkgs; [
      terraform-full
      terracognita
    ];
  };
}
