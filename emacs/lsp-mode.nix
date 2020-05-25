{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.lsp-mode = {
    demand = true;
    commands = [ "lsp" ];
    config = ''
    '';
  };
}
