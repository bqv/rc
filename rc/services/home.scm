(define-module (rc services home)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages guile)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (home-configuration
            home-configuration?
            home-service-type))

(define-record-type* <home-configuration>
  home-configuration make-home-configuration
  home-configuration? this-home-configuration
  (user      home-configuration-user     ;string
             (default #f))
  (group     home-configuration-group    ;string
             (default "users"))
  (profile   home-configuration-profile  ;string
             (default #f)))

(define home-shepherd-service
  (match-lambda
    (($ <home-configuration> user group profile)
     (list
      (shepherd-service
       (provision (list (string->symbol (string-append "home-" user))))
       (documentation "Run home shepherd.")
       (requirement '())
       (start #~(lambda _
                  (fork+exec-command
                    (let ((launch-exp
                            #$((@@ (gnu home-services shepherd) launch-shepherd-gexp)
                               (service-value
                                 (car (filter
                                        (lambda (s) (eq? (service-type-name (service-kind s))
                                                         'home-shepherd))
                                        ((@ (gnu home) home-environment-services)
                                         ((@ (rc home) leaf) (@ (rc system) delta)))))))))
                      (list #$(file-append guile-3.0 "/bin/guile")
                            "--no-auto-compile"
                            #$(string-append (or profile
                                                 (string-append "/home/" user
                                                                "/.guix-home"))
                                             "/on-first-login")))
                    #:user #$user
                    #:group #$group
                    #:environment-variables
                    (append (list #$(string-append "HOME="
                                                   (passwd:dir (getpw user)))
                                  "SSL_CERT_DIR=/etc/ssl/certs"
                                  "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt")))
                  #t))
       (one-shot? #t)
       (respawn? #f)
       (stop #~(make-kill-destructor)))))))

(define home-service-type
  (service-type (name 'home)
                (extensions (list (service-extension shepherd-root-service-type
                                                     home-shepherd-service)))
                (description "Run home.")))
