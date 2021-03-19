{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.jabber = {
    demand = true;
    package = epkgs: epkgs.jabber.overrideAttrs (drv: {
      src = pkgs.fetchGit {
        url = "https://tildegit.org/wgreenhouse/emacs-jabber.git";
        rev = "master";
        sha256 = "null";
      };
    });
    config = ''
      nil
    '';
  };
}
