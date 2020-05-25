{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.typescript-mode = {
    demand = true;
    after = [ "lsp" ];
    hook = [
      { typescript-mode = "lsp"; }
    ];
  };
}
