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
      { csharp-mode-hook = "omnisharp-mode"; }
    ];
    config = ''
      (setq lsp-csharp-server-path "${pkgs.omnisharp-roslyn}/bin/omnisharp")

      (defun bqv/csharp-mode-setup ()
        (setq indent-tabs-mode nil
              c-syntactic-indentation t)
        (c-set-style "ellemtel")
        (setq c-basic-offset 4
              truncate-lines t
              tab-width 4
              evil-shift-width 4))

      (add-hook 'csharp-mode-hook #'bqv/csharp-mode-setup)
    '';
  };
}
