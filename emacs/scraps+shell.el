(setq explicit-shell-file-name (executable-find "bash"))
(setq shell-file-name (executable-find "bash"))
(do-buffers (setenv "PAGER" "cat"))
