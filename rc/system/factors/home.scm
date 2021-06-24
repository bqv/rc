(define-module (rc system factors home)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu packages admin)
  #:export (apply-home-services))

(define* (apply-home-services #:rest envs)
  (define (add-services services)
    (cons*
      services))

  add-services)
