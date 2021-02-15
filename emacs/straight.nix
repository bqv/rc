{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.straight = {
    demand = true;
    commands = [ "straight-use-package" ];
    package = epkgs: epkgs.trivialBuild rec {
      pname = "straight";
      version = src.shortRev;
      src = pkgs.withSources.emacs-straight;
    };
  };
}
