{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.jabber = {
    demand = true;
    package = epkgs: epkgs.trivialBuild rec {
      pname = "jabber";
      version = lib.substring 0 7 src.rev;
      src = pkgs.fetchzip {
        url = "https://gitlab.com/cnngimenez/emacs-jabber/-/archive/lexical-binding/emacs-jabber-lexical-binding.tar.gz";
        sha256 = "1vg5zwcc7afwbjbravbbwjbfpd3jz2i4zckmnlpg5cabidlxwjzr";
      };
      buildInputs = with epkgs; [
        srv fsm
      ];
    };
    config = ''
      nil
    '';
  };
}
