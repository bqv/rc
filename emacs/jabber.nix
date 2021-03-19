{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.jabber = {
    demand = true;
    package = epkgs: epkgs.jabber.overrideAttrs (drv: {
      src = "https://tildegit.org/wgreenhouse/emacs-jabber.git";
    });
    config = ''
      nil
    '';
  };
}
