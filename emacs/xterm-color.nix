{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.xterm-color = {
    demand = true;
    config = ''
      (when false
        (setq comint-output-filter-functions
                (remove 'ansi-color-process-output comint-output-filter-functions))
        (add-hook 'shell-mode-hook
                (lambda ()
                    ;; Disable font-locking in this buffer to improve performance
                    (font-lock-mode -1)
                    ;; Prevent font-locking from being re-enabled in this buffer
                    (make-local-variable 'font-lock-function)
                    (setq font-lock-function (lambda (_) nil))
                    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))
      ); Disabled - we just use vterm now

      (require 'eshell)
      (defun eshell/erase ()
        "Clear the eshell buffer."
        (interactive)
        (let ((eshell-buffer-maximum-lines 0)) (eshell-truncate-buffer)))
      (defun bqv/eshell-define-bol ()
        (defun bqv/eshell-maybe-bol ()
          (interactive)
          (let ((p (point)))
            (eshell-bol)
            (if (= p (point))
                (beginning-of-line))))
        (define-key eshell-mode-map "\C-a" 'bqv/eshell-maybe-bol)
        (define-key eshell-mode-map "<home>" 'bqv/eshell-maybe-bol))
      (defun bqv/eshell-nix-env ()
        (let* ((userenv (shell-command-to-string ". /etc/profile.d/nix{,-daemon,}.sh; set"))
               (envlist (let ((list))
                          (dolist (kv (split-string userenv "\n") list)
                            (setq list (cons (split-string kv "=") list)))))
               (needle (regexp-quote "NIX")))
          (dolist (env envlist nil)
            (if (or (string-match-p needle (car env)) (string= "PATH" (car env)))
                (setenv (car env) (string-join (cdr env) "="))))))
      (defun bqv/eshell-pager ()
        (setenv "PAGER" "cat"))
      (defun bqv/eshell-visual ()
        (dolist (command '("watch" "htop") nil)
          (add-to-list 'eshell-visual-commands command)))
      (add-hook 'eshell-mode-hook 'bqv/eshell-define-bol)
      (add-hook 'eshell-mode-hook 'bqv/eshell-nix-env)
      (add-hook 'eshell-mode-hook 'bqv/eshell-pager)
      (add-hook 'eshell-mode-hook 'bqv/eshell-visual)

      (when false
        (add-hook 'eshell-before-prompt-hook
                (lambda ()
                    (setq xterm-color-preserve-properties t)))
        (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
        (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
        (setq compilation-environment '("TERM=xterm-256color"))
        (add-hook 'compilation-start-hook
                (lambda (proc)
                    ;; We need to differentiate between compilation-mode buffers
                    ;; and running as part of comint (which at this point we assume
                    ;; has been configured separately for xterm-color)
                    (when (eq (process-filter proc) 'compilation-filter)
                    ;; This is a process associated with a compilation-mode buffer.
                    ;; We may call `xterm-color-filter' before its own filter function.
                    (set-process-filter
                    proc
                    (lambda (proc string)
                        (funcall 'compilation-filter proc
                                (xterm-color-filter string)))))))
      ); Disabled - aaaaa?
    '';
  };
}
