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
