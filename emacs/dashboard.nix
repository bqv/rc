{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.dashboard = {
    demand = true;
    config = ''
      (dashboard-setup-startup-hook)
      (setq dashboard-center-content t)
      (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
    '';
  };
}
