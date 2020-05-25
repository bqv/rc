{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.lsp-ui = {
    demand = true;
    after = [ "lsp-mode" ];
  };
}
