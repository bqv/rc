(defun bqv/pmbootstrap-log ()
  "Open pmbootstrap log in a buffer."
  (interactive)
  (switch-to-buffer
   (let* ((title "*pmbootstrap-log*")
          (buffer (get-buffer title)))
     (or buffer
         (save-excursion
           (with-current-buffer (window-buffer
                                 (async-shell-command "pmbootstrap log"))
             (rename-buffer title t)))))))

(defun comint-sudo-run (program)
  "Run sudo PROGRAM in a Comint buffer and switch to it.
See `comint-run'."
  (declare (interactive-only make-comint))
  (interactive "sRun program: ")
  (let ((name (file-name-nondirectory program)))
    (switch-to-buffer (make-comint name "sudo" nil program))
    (run-hooks (intern-soft (concat "comint-" name "-hook")))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))
