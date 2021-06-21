(define-module (rc system)
  #:use-module (rc system delta)
  #:use-module (rc system epsilon)
  #:use-module (srfi srfi-1)
  #:export (hosts
            systems
            current-host
            current-system

            delta
            epsilon))

(define hosts
  (hash-fold (lambda (k v p) (cons k p))
	     '()
	     (module-submodules (current-module))))

(define-public systems 
  (filter-map
    (lambda (host)
      (if (eq? host 'factors) #f
          (cons (symbol->string host)
                (module-ref (hashq-ref (module-submodules (current-module)) host) 'os))))
    hosts))

(define (current-host) (gethostname))

(define (current-system) ((assoc-ref systems (current-host))))

(define (delta) ((assoc-ref systems "delta")))
(define (epsilon) ((assoc-ref systems "epsilon")))
