{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.flycheck-posframe = {
    demand = true;
    after = [ "flycheck" "lsp-mode" ];
    hook = [
   #  { flycheck-mode-hook = "flycheck-posframe-mode"; }
    ];
    config = ''
      (defun turn-off-lsp-ui (&rest r)
        (lsp-ui-mode -1))
      (add-to-list 'lsp-mode-hook #'turn-off-lsp-ui)
    '';
  };
}
