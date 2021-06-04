(define-module (rc system))

(define-public systems 
  (map (lambda (f) (string-drop-right f 4))
       (cddr (reverse
	       (let ((s (opendir (string-drop-right (current-filename) 4)))
		     (l '()))
		 (do ((d (readdir s) (readdir s)))
		   ((eof-object? d))
		   (set! l (cons d l)))
		 (closedir s)
		 l)))))

(do ((rest systems (cdr rest)))
  ((nil? rest))
  (let ((sys (string->symbol (car rest))))
    (eval `(define-public (,sys) ((@ (rc system ,sys) os)))
	  (interaction-environment))))
