{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.lui = {
    demand = true;
    package = epkgs: [ epkgs.circe ];
  };
}
