(setq explicit-shell-file-name (executable-find "bash"))
(setq shell-file-name (executable-find "bash"))
(setq async-shell-command-buffer 'confirm-rename-buffer)
(setq async-shell-command-display-buffer nil)
(do-buffers (setenv "PAGER" "cat"))

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
