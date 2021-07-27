{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.lsp-ui = {
    demand = true;
    after = [ "lsp-mode" ];
    config = ''
      (setq lsp-ui-doc-show-with-cursor t) ; disable for zero sluggishness
      (setq lsp-ui-doc-delay 2) ; get's rid of most of the sluggishness
    '';
  };
}
