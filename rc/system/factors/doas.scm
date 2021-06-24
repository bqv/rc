(define-module (rc system factors doas)
 ;#:use-module (srfi srfi-28)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu packages admin)
  #:export (use-doas-services))

(define-record-type* <doas-configuration>
  doas-configuration make-doas-configuration
  doas-configuration?
  (doas doas-configuration-doas (default opendoas))
  (rules doas-configuration-rules
         (default '("permit :wheel"))))

(define (use-doas-services services)
  (cons*
    (extra-special-file
      "/etc/doas.conf"
      (plain-file "doas.conf"
                  (string-join (list
                                 "permit nopass keepenv root" ; allowed to do anything
                                 "permit nopass setenv { SSH_AUTH_SOCK IPFS_PATH } :wheel"
                                 "")
                               "\n")))
    (simple-service 'doas-profile-service profile-service-type
                    (list opendoas))
    (simple-service 'doas-setuid-service setuid-program-service-type
                    (list #~(string-append #$opendoas "/bin/doas")))
    services))
