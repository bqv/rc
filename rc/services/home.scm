(define-module (rc services home)
  #:use-module (gnu services)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export ())

(define null-service-type
  (service-type
    (name 'null)
    (extensions (list))
    (default-value #f)
    (description "")))
(simple-service 'nil null-service-type #t)
