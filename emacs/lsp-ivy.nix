{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.lsp-ivy = {
    demand = true;
    after = [ "ivy" "lsp-mode" ];
  };
}
