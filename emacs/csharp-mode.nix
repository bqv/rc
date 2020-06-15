{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.csharp-mode = {
    demand = true;
    package = epkgs: epkgs.csharp-mode.overrideAttrs (_: {
      postInstall = ''
        find $out -iname '*.elc' -delete
      '';
    });
  };
}
