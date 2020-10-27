{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.purescript-mode = {
    demand = true;
    after = [ "lsp-mode" ];
    hook = [
      { purescript-mode-hook = "lsp"; }
    ];
  };
}
