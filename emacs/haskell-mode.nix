{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.haskell-mode = {
    demand = true;
    after = [ "lsp" ];
    hook = [
      { haskell-mode = "lsp"; }
    ];
    config = ''
      (add-to-list 'auto-mode-alist '("\\.tpl" . mhtml-mode))
    '';
  };
}
