{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.rust-mode = {
    demand = true;
    after = [ "lsp" ];
    hook = [
      { rust-mode = "lsp"; }
    ];
  };
}
