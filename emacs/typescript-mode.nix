{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.typescript-mode = {
    demand = true;
    after = [ "lsp-mode" ];
    hook = [
      { typescript-mode-hook = "lsp"; }
    ];
  };
}
