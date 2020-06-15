{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.csharp-mode = {
    demand = true;
    package = epkgs: epkgs.csharp-mode.overrideAttrs (_: {
      postInstall = ''
        find $out -iname '*.elc' -delete
      '';
    });
    hook = [
      { csharp-mode-hook = "lsp"; }
    ];
    config = ''
      (setq lsp-csharp-server-path "${pkgs.omnisharp-roslyn}/bin/omnisharp")
    '';
  };
}
