(define-module (gnu s6-services rc)
  #:use-module (gnu s6-services)
  #:use-module (gnu packages skarnet)
  #:use-module (gnu services s6-rc)
  #:use-module (guix sets)
  #:use-module (guix gexp)
  #:use-module (guix records)

  #:use-module (srfi srfi-1)

  #:re-export (s6-rc-service
               s6-rc-action))

(define-record-type* <s6-rc-configuration>
  s6-rc-configuration make-s6-rc-configuration
  s6-rc-configuration?
  (s6-rc s6-rc-configuration-package
         (default s6-rc)) ; package
  (auto-start? s6-rc-configuration-auto-start?
               (default #t))
  (services s6-rc-configuration-services
            (default '())))

(define (s6-rc-configuration-file services rc)
  "Return the rc configuration file for SERVICES.  S6 is used
as rc package."
  (assert-valid-graph services)

  (let ((files (map s6-rc-service-file services))
        ;; TODO: Add compilation of services, it can improve start
        ;; time.
        ;; (scm->go (cute scm->go <> rc))
        )
    (define config
      #~(begin
          (use-modules (srfi srfi-34)
                       (system repl error-handling))
          (apply
           register-services
           (map
            (lambda (file) (load file))
            '#$files))
          (action 'root 'daemonize)
          (format #t "Starting services...~%")
          (for-each
           (lambda (service) (start service))
           '#$(append-map s6-rc-service-provision
                          (filter s6-rc-service-auto-start?
                                  services)))
          (newline)))

    (scheme-file "s6-rc.conf" config)))

(define (launch-rc-gexp config)
  (let* ((s6-rc (s6-rc-configuration-package config))
         (services (s6-rc-configuration-services config)))
    (if (s6-rc-configuration-auto-start? config)
        (with-imported-modules '((guix build utils))
          #~(let ((log-dir (or (getenv "XDG_LOG_HOME")
                               (format #f "~a/.local/var/log" (getenv "HOME")))))
              ((@ (guix build utils) mkdir-p) log-dir)
              (system*
               #$(file-append s6-rc "/bin/rc")
               "--logfile"
               (string-append
                log-dir
                "/s6-rc.log")
               "--config"
               #$(s6-rc-configuration-file services s6-rc))))
        #~"")))

(define (reload-configuration-gexp config)
  (let* ((s6-rc (s6-rc-configuration-package config))
         (services (s6-rc-configuration-services config)))
    #~(system*
       #$(file-append s6-rc "/bin/herd")
       "load" "root"
       #$(s6-rc-configuration-file services s6-rc))))

(define (ensure-rc-gexp config)
  #~(if (file-exists?
         (string-append
          (or (getenv "XDG_RUNTIME_DIR")
              (format #f "/run/user/~a" (getuid)))
          "/s6-rc/socket"))
        #$(reload-configuration-gexp config)
        #$(launch-rc-gexp config)))

(define-public s6-rc-service-type
  (service-type (name 's6-rc)
                (extensions
                 (list (service-extension
                        s6-run-on-first-login-service-type
                        launch-rc-gexp)
                       (service-extension
                        s6-activation-service-type
                        ensure-rc-gexp)
                       (service-extension
                        s6-profile-service-type
                        (lambda (config)
                          `(,(s6-rc-configuration-package config))))))
                (compose concatenate)
                (extend
                 (lambda (config extra-services)
                   (s6-rc-configuration
                    (inherit config)
                    (services
                     (append (s6-rc-configuration-services config)
                             extra-services)))))
                (default-value (s6-rc-configuration))
                (description "Configure and install userland s6-rc")))


