(setq explicit-shell-file-name (executable-find "bash"))
(setq shell-file-name (executable-find "bash"))
(do-buffers (setenv "PAGER" "cat"))

(defun shell-region (start end)
  "execute region in an inferior shell"
  (interactive "r")
  (shell-command (buffer-substring-no-properties start end)))
(global-set-key (kbd "C-x !") #'shell-region)
