{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.dap-mode = {
    demand = true;
    after = [ "lsp-mode" ];
  };
}
