(setq explicit-shell-file-name (executable-find "bash"))
(setq shell-file-name (executable-find "bash"))
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

(setq enable-recursive-minibuffers t)
(defun ivy-shell (&optional async &key initial-input)
  (ivy-read (format "[%s] shell: " (if async "async" "synch"))
            shell-command-history
            :caller 'ivy-shell
            :keymap (let ((keymap (make-keymap)))
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
                      keymap)
            :action (if async #'async-shell-command #'shell-command)
            :initial-input (or initial-input "")
            :history 'shell-command-history))
(defun ivy-shell-sync (&optional pfx)
  (interactive "p")
  (ivy-shell nil :initial-input (if (= pfx -1) (car shell-command-history))))
(global-set-key (kbd "M-!") #'ivy-shell-sync)
(defun ivy-shell-async (&optional pfx)
  (interactive "p")
  (ivy-shell t :initial-input (if (= pfx -1) (car shell-command-history))))
(global-set-key (kbd "M-&") #'ivy-shell-async)

(provide 'scraps+shell)
;;; scraps+shell.el ends here
