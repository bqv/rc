{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.sln-mode = {
    demand = true;
  };
}
