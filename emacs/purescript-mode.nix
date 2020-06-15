{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.purescript-mode = {
    demand = true;
    after = [ "lsp" ];
    hook = [
      { purescript-mode-hook = "lsp"; }
    ];
  };
}
