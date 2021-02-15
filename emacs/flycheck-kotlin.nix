{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.flycheck-kotlin = {
    demand = true;
  };
}
