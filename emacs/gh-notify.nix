{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.gh-notify = {
    demand = true;
    after = [ "magit" "forge" ];
    config = ''
      (with-eval-after-load 'evil
        (add-to-list 'evil-emacs-state-modes 'gh-notify-mode))
    '';
  };
}
