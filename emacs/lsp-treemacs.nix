{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.lsp-treemacs = {
    demand = true;
    after = [ "treemacs" "lsp-mode" ];
  };
}
