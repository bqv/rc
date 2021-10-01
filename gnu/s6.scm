(define-module (gnu s6)
  #:use-module (gnu s6-services)
  #:use-module (gnu s6-services symlink-manager)
  #:use-module (gnu s6-services shells)
  #:use-module (gnu s6-services xdg)
  #:use-module (gnu s6-services fontutils)
  #:use-module (gnu services)
  #:use-module (guix records)
  #:use-module (guix diagnostics)

  #:export (s6-environment
            s6-environment?
            this-s6-environment

            s6-environment-derivation
            s6-environment-user-services
            s6-environment-essential-services
            s6-environment-services
            s6-environment-location

            s6-environment-with-provenance))

;;; Comment:
;;;
;;; This module provides a <s6-environment> record for managing
;;; per-user packages and configuration files in the similar way as
;;; <operating-system> do for system packages and configuration files.
;;;
;;; Code:

(define-record-type* <s6-environment> s6-environment
  make-s6-environment
  s6-environment?
  this-s6-environment

  (packages s6-environment-packages             ; list of (PACKAGE OUTPUT...)
            (default '()))

  (essential-services s6-environment-essential-services ; list of services
                      (thunked)
                      (default (s6-environment-default-essential-services
                                this-s6-environment)))
  (services s6-environment-user-services
            (default '()))

  (location s6-environment-location             ; <location>
            (default (and=> (current-source-location)
                            source-properties->location))
            (innate)))

(define (s6-environment-default-essential-services he)
  "Return the list of essential services for s6 environment."
  (list
   (service s6-run-on-first-login-service-type)
   (service s6-activation-service-type)
   (service s6-environment-variables-service-type)

   (service s6-symlink-manager-service-type)

   (service s6-fontconfig-service-type)
   (service s6-xdg-base-directories-service-type)
   (service s6-shell-profile-service-type)

   (service s6-service-type)
   (service s6-profile-service-type (s6-environment-packages he))))

(define* (s6-environment-services he)
  "Return all the services of s6 environment."
  (instantiate-missing-services
   (append (s6-environment-user-services he)
           (s6-environment-essential-services he))))

(define* (s6-environment-derivation he)
  "Return a derivation that builds OS."
  (let* ((services         (s6-environment-services he))
         (s6 (fold-services services
                              #:target-type s6-service-type)))
    (service-value s6)))

(define* (s6-environment-with-provenance he config-file)
  "Return a variant of HE that stores its own provenance information,
including CONFIG-FILE, if available.  This is achieved by adding an instance
of HOME-PROVENANCE-SERVICE-TYPE to its services."
  (s6-environment
    (inherit he)
    (services (cons (service s6-provenance-service-type config-file)
                    (s6-environment-user-services he)))))
