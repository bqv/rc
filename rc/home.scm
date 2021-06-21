(define-module (rc home)
  #:use-module (rc home leaf)
  #:use-module (srfi srfi-1)
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
  (filter-map
    (lambda (user)
      (if (eq? user 'factors) #f
          (cons (symbol->string user)
                (module-ref (hashq-ref (module-submodules (current-module)) user) 'env))))
    users))

(define (current-user) (getlogin))

(define (current-home) ((assoc-ref homes (current-user))))

(define (leaf os) ((assoc-ref homes "leaf") os))
