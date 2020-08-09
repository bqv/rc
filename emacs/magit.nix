{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.magit = {
    demand = true;
    bind = {
      "C-x g" = "magit-status";
      "C-x M-g" = "magit-dispatch-popup";
      "C-c g" = "magit-dispatch";
    };
    config = ''
      (setq magit-log-show-refname-after-summary t)
      (setq magit-clone-default-directory (expand-file-name "~/dev/"))
      (setq ediff-window-setup-function 'ediff-setup-windows-plain)
      (setq magit-section-initial-visibility-alist nil)
      (setq magit-wip-merge-branch t)
      (magit-wip-mode)
    '';
    systemDeps = with pkgs; [ git ];
  };
}
