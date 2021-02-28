{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.treemacs-magit = {
    demand = true;
    after = [ "treemacs" "magit" ];
    config = ''
      (defun ivy-shell-keymap (&optional async &key initial-input)
      (let ((keymap (make-keymap)))
          (define-key keymap (kbd "<RET>") `(lambda ()
                                              (interactive)
                                              (if (string-equal (ivy--input) "")
                                                  (ivy-done)
                                              (ivy-immediate-done))))
          (define-key keymap (kbd "<tab>") #'completion-at-point)
          (define-key keymap (kbd "C-i") `(lambda ()
                                          (interactive)
                                          (add-to-list 'completion-at-point-functions
                                                      'comint-completion-at-point)
                                          (completion-at-point)))
          (define-key keymap (kbd "<backtab>") #'ivy-insert-current)
          (define-key keymap (kbd "<C-tab>") `(lambda ()
                                              (interactive)
                                              (ivy-quit-and-run
                                                  (ivy-shell ,(not async)
                                                          :initial-input ,initial-input))))
          keymap))

      (setq enable-recursive-minibuffers t)
      (defun ivy-shell (&optional async &key initial-input)
      (ivy-read (format "[%s] shell: " (if async "async" "synch"))
                  shell-command-history
                  :caller 'ivy-shell
                  :keymap (ivy-shell-keymap async :initial-input initial-input)
                  :action (if async #'async-shell-command #'shell-command)
                  :initial-input (or initial-input "")
                  :history 'shell-command-history))
      (defun ivy-shell-sync (&optional pfx)
      (interactive "p")
      (minibuffer-with-setup-hook
          (lambda ()
              (shell-completion-vars)
              (setq-local minibuffer-default-add-function
                          #'minibuffer-default-add-shell-commands))
          (ivy-shell nil :initial-input (if (= (or pfx 1) -1) (car shell-command-history)))))
      (global-set-key (kbd "M-!") #'ivy-shell-sync)
      (global-set-key (kbd "´") #'ivy-shell-sync)
      (evil-define-key 'normal 'global (kbd "´") #'ivy-shell-sync)
      (defun ivy-shell-async (&optional pfx)
      (interactive "p")
      (minibuffer-with-setup-hook
          (lambda ()
              (shell-completion-vars)
              (setq-local minibuffer-default-add-function
                          #'minibuffer-default-add-shell-commands))
      (ivy-shell t :initial-input (if (= (or pfx 1) -1) (car shell-command-history)))))
      (global-set-key (kbd "M-&") #'ivy-shell-async)
      (global-set-key (kbd "`") #'ivy-shell-async)
      (evil-define-key 'normal 'global (kbd "`") #'ivy-shell-async)

      (defun ivy-term (&optional &key initial-input)
      (interactive)
      (ivy-read (format "[%s] term: " "vterm")
                  shell-command-history
                  :caller 'ivy-term
                  :keymap (ivy-shell-keymap :initial-input initial-input)
                  :action #'vterm-shell-command
                  :initial-input (or initial-input "")
                  :history 'shell-command-history))
      (defalias #'ivy-vterm #'ivy-term)
      (global-set-key (kbd "C-x M-&") #'ivy-term)

      (define-transient-command nixos-dispatch ()
        ["Arguments"
         ("-t" "show trace" "--show-trace")
         (nixos:--tarball-ttl)]
        ["NixOS"
         ("r" "rebuild" nixos-rebuild)
         ("o" "search options" nixos-search-options)
         ("p" "search packages" nixos-search-packages)
         ("h" "check channels" nixos-howoldis)
         ("i" "index" nixos-index)]
        ["Garbage collection"
         ("g" "collect garbage" nixos-garbage-collect-dispatch)]
        ["Nix Store"
         ("s v" "verify" nix-store-verify)
         ("s r" "repair" nix-store-repair)
         ("s d" "query dependents" nix-store-query-dependents)
         ("s k" "delete" nix-store-delete)
         ("s o" "optimize" nix-store-optimize)])

      (define-transient-command nixos-garbage-collect-dispatch ()
        ["Garbage collection"
         (nixos:--delete-older-than)
         ("g" "collect garbage" nixos-garbage-collect)])
    '';
  };
}
