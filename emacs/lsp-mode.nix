{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.lsp-mode = {
    demand = true;
    package = epkgs: epkgs.lsp-mode.overrideAttrs (_: {
      postInstall = ''
        cd $out/share/emacs/site-lisp/elpa/*
        mkdir snippets
      '';
    });
    commands = [ "lsp" ];
    config = ''
    '';
  };
}
