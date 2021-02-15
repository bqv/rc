{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.counsel = {
    demand = true;
    after = [ "ivy" ];
    bind = {
      "M-x" = "counsel-M-x";
      "C-x M-f" = "counsel-recentf";
     #"C-y" = "counsel-yank-pop";
    };
    config = ''
      (setq ivy-initial-inputs-alist nil)
      (recentf-mode 1)
      (setq recentf-max-menu-items 64)
      (setq recentf-max-saved-items 64)
      (run-at-time nil (* 5 60) 'recentf-save-list)

      (defun suppress-messages (func &rest args)
        (let ((save-silently t))
          (apply func args))
        (message nil))
      (advice-add 'recentf-save-list :around #'suppress-messages)
    '';
    systemDeps = with pkgs; [ ag ];
  };
}
