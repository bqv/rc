(define-module (rc services biboumi)
  #:use-module (rc packages biboumi)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (biboumi-configuration
            biboumi-configuration?
            biboumi-service-type))

(define-record-type* <biboumi-configuration>
  biboumi-configuration make-biboumi-configuration
  biboumi-configuration?
  (biboumi   biboumi-configuration-biboumi   ;<package>
             (default biboumi))
  (name      biboumi-configuration-name      ;string
             (default #f))
  (config    biboumi-configuration-file      ;<file>
             (default (plain-file "biboumi.cfg" "")))
  (user      biboumi-configuration-user      ;string
             (default #f))
  (group     biboumi-configuration-group     ;string
             (default "users"))
  (home      biboumi-configuration-home      ;string
             (default #f)))

(define biboumi-shepherd-service
  (match-lambda
    (($ <biboumi-configuration> biboumi name config user group home)
     (list
      (shepherd-service
       (provision (list (string->symbol (if name
                                            (string-append "biboumi-" name)
                                            "biboumi"))))
       (documentation "Run biboumi.")
       (requirement '(networking))
       (start #~(lambda _
                  (mkdir-p "/var/lib/biboumi")
                  (with-output-to-file "/var/log/biboumi.log"
                                       (lambda _ (newline)))
                  (let ((pwd (getpwnam "biboumi")))
                    (chown "/var/log/biboumi.log" (passwd:uid pwd) (passwd:gid pwd))
                    (chown "/var/lib/biboumi" (passwd:uid pwd) (passwd:gid pwd)))
                  (fork+exec-command
                    (list #$(file-append biboumi "/bin/biboumi")
                          #$config)
                    #:user #$user
                    #:group #$group
                    #:log-level "/var/log/biboumi.log"
                    #:environment-variables
                    (append (list (string-append "HOME="
                                                 (or #$home (passwd:dir (getpw #$user))))
                                  "SSL_CERT_DIR=/etc/ssl/certs"
                                  "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt")
                            (remove (lambda (str)
                                      (or (string-prefix? "HOME=" str)
                                          (string-prefix? "SSL_CERT_DIR=" str)
                                          (string-prefix? "SSL_CERT_FILE=" str)))
                                    (environ))))))
       (respawn? #t)
       (stop #~(make-kill-destructor)))))))

(define biboumi-service-type
  (service-type (name 'biboumi)
                (extensions (list (service-extension shepherd-root-service-type
                                                     biboumi-shepherd-service)))
                (description "Run biboumi.")))
