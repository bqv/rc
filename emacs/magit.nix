{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.magit = {
    demand = true;
    bind = {
      "C-x g" = "magit-status";
      "C-x M-g" = "magit-dispatch-popup";
    };
    config = ''
      (setq magit-log-show-refname-after-summary t)
      (setq magit-clone-default-directory (expand-file-name "~/dev/"))
      (setq ediff-window-setup-function 'ediff-setup-windows-plain)
      (setq magit-wip-merge-branch t)
      (magit-wip-mode)
    '';
    systemDeps = with pkgs; [ git ];
  };
}
