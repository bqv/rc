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
