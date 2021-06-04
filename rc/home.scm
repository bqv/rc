(define-module (rc home)
	       #:use-module (rc home leaf)
	       #:export (users
			 homes
			 current-user
			 current-home
			 
			 leaf))

(define users
  (hash-fold (lambda (k v p) (cons k p))
	     '()
	     (module-submodules (current-module))))

(define-public homes 
  (map (lambda (user)
	 (cons (symbol->string user)
	       (module-ref (hashq-ref (module-submodules (current-module)) user) 'env)))
       users))

(define (current-user) (getlogin))

(define (current-home) ((assoc-ref homes (current-user))))

(define (leaf) ((assoc-ref homes "leaf")))
