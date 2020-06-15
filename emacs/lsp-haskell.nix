{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.lsp-haskell = {
    demand = true;
    after = [ "lsp" ];
    hook = [
      { haskell-mode-hook = "lsp"; }
    ];
    config = ''
      (setq lsp-haskell-process-path-hie "hie-wrapper")
    '';
  };
}
