(setq explicit-shell-file-name (executable-find "bash"))
(setq shell-file-name (executable-find "bash"))
(setq async-shell-command-buffer 'confirm-rename-buffer)
(setq async-shell-command-display-buffer nil)

(do-buffers (setenv "PAGER" "cat")) ; depends: ace-window-config

(defun comint-sudo-run (program)
  "Run sudo PROGRAM in a Comint buffer and switch to it.
See `comint-run'."
  (declare (interactive-only make-comint))
  (interactive "sRun program: ")
  (let ((name (file-name-nondirectory program)))
    (switch-to-buffer (make-comint name "sudo" nil program))
    (run-hooks (intern-soft (concat "comint-" name "-hook")))))

(defun shell-region (start end)
  "execute region in an inferior shell"
  (interactive "r")
  (shell-command (buffer-substring-no-properties start end)))
(global-set-key (kbd "C-x !") #'shell-region)

(defun async-shell-region (start end)
  "execute region in an inferior shell"
  (interactive "r")
  (async-shell-command (buffer-substring-no-properties start end)))
(global-set-key (kbd "C-x &") #'async-shell-region)

(provide 'scraps+shell)
;;; scraps+shell.el ends here
