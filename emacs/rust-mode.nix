{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.rust-mode = {
    demand = true;
    after = [ "lsp-mode" ];
    hook = [
      { rust-mode-hook = "lsp"; }
    ];
    systemDeps = with pkgs; [ rls rust-analyzer ];
  };
}
