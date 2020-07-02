{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.company-lsp = {
    demand = true;
    after = [ "company" "lsp" ];
    config = ''
      (push 'company-lsp company-backends)
      (setq +lsp-company-backend 'company-capf)
    '';
  };
}
