{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.csproj-mode = {
    demand = true;
  };
}
