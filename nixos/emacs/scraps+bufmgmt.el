(defun advice-list (sym)
  "Get advice set for SYM."
  (let ((l nil))
    (advice-mapc (lambda (a &rest ignored)
                   (push a l))
                 sym)
    l))

(defmacro do-buffers (&rest exprs)
  "Do EXPRS for every buffer."
  `(dolist (buf (buffer-list))
     (with-current-buffer buf ,@exprs)))

(defun setenv-global (key value)
  "Set KEY to VALUE in every buffer."
  (do-buffers (setenv key value)))
