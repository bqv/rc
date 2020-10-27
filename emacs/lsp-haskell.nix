{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.lsp-haskell = {
    demand = true;
    after = [ "lsp" "haskell-mode" ];
    hook = [
      { haskell-mode-hook = "lsp"; }
    ];
    config = ''
      (add-to-list 'haskell-mode-hook #'lsp)
     ;(add-to-list 'after-init-hook (lambda (&rest r) (add-to-list 'haskell-mode-hook 'lsp)))
     ;(setq lsp-haskell-server-path "${pkgs.haskellPackages.ghcide}/bin/ghcide")
     ;(setq lsp-haskell-server-args nil)
      (setq lsp-haskell-process-path-hie "ghcide")
      (setq lsp-haskell-process-args-hie nil)
    '';
  };
}
