{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.evil = {
    demand = true;
    diminish = [ "undo-tree-mode" ];
    config = ''
      (setq evil-default-cursor t)
      ;(advice-add 'global-set-key :after
      ;            (lambda (old-fn &rest args)
      ;                     (apply 'evil-global-set-key args)))
      (evil-set-initial-state 'weechat-mode 'emacs)
      (evil-set-initial-state 'exwm-mode 'emacs)
      (evil-set-initial-state 'eshell-mode 'emacs)
      (evil-set-initial-state 'term-mode 'emacs)
      (evil-set-initial-state 'vterm-mode 'emacs)
      (evil-set-initial-state 'dashboard-mode 'emacs)
      (evil-mode 1)
      (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
      (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
      (define-key evil-insert-state-map (kbd "C-w") 'kill-region)
      (define-key evil-visual-state-map (kbd "C-e") 'move-end-of-line)
      (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
      (define-key evil-normal-state-map (kbd "C-k") 'kill-line)
      (define-key evil-normal-state-map (kbd "C-y") 'yank)
      (define-key evil-insert-state-map (kbd "C-y") 'yank)
      (define-key evil-normal-state-map (kbd "C-w") 'kill-region)
      (define-key evil-visual-state-map (kbd "C-w") 'kill-region)
      
      ;; Override :q to kill buffer only
      (evil-define-command evil-quit (&optional force)
        "Close the current buffer (Overridden)."
        :repeat nil
        (interactive "<!>")
        (kill-buffer))
    '';
  };
}
