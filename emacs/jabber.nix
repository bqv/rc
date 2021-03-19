{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.jabber = {
    demand = true;
    package = epkgs: epkgs.jabber.overrideAttrs (drv: {
      src = pkgs.fetchgit {
        url = "https://tildegit.org/wgreenhouse/emacs-jabber.git";
        rev = "69d6122858f036958323c50f81423389b0f91d69";
        sha256 = "0vg5zwcc7afwbjbravbbwjbfpd3jz2i4zckmnlpg5cabidlxwjzr";
        # date = 2021-03-16T20:16:52-04:00;
      };
    });
    config = ''
      nil
    '';
  };
}
