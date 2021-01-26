{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.eterm-256color = {
    demand = true;
    config = ''
      (add-hook 'term-mode-hook #'eterm-256color-mode)
      (setenv "TMUX" "off")
      (with-demoted-errors (dolist (buffer (buffer-list))
                             (with-current-buffer buffer
                               (setenv "TMUX" "off"))))
      (defun bqv/nixos-switch ()
        (interactive)
        (bqv/exwm-sudo-exec "nixos-rebuild switch --show-trace"))
      (defun bqv/nixos-dry-switch ()
        (interactive)
        (bqv/exwm-sudo-exec "nixos-rebuild dry-activate --show-trace"))
    '';
  };
}
