{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.gh-notify = {
    demand = true;
    after = [ "magit" "forge" ];
    config = ''
      (add-to-list 'evil-emacs-state-modes 'gh-notify-mode)
    '';
  };
}
