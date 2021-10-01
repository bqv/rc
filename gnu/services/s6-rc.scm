(define-module (gnu services s6-rc)
  #:use-module (guix ui)
  #:use-module (guix sets)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix records)
  #:use-module (guix derivations)                 ;imported-modules, etc.
  #:use-module (guix utils)
  #:use-module (gnu s6-services)
  #:use-module (gnu services)
  #:use-module (gnu services herd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages skarnet)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (s6-rc-configuration
            s6-rc-configuration?
            s6-rc-configuration-package
            s6-rc-configuration-services

            s6-rc-root-service-type
            %s6-rc-root-service
            s6-rc-service-type

            s6-rc-home-service-type
            s6-rc-launch-home-gexp
            s6-rc-reload-home-gexp
            s6-rc-ensure-home-gexp

            s6-rc-service
            s6-rc-service?
            s6-rc-service-documentation
            s6-rc-service-provision
            s6-rc-service-canonical-name
            s6-rc-service-requirement
            s6-rc-service-one-shot?
            s6-rc-service-respawn?
            s6-rc-service-start
            s6-rc-service-stop
            s6-rc-service-auto-start?
            s6-rc-service-modules

            s6-rc-action
            s6-rc-action?
            s6-rc-action-name
            s6-rc-action-documentation
            s6-rc-action-procedure

            %default-modules

            s6-rc-service-dir

            s6-rc-service-lookup-procedure
            s6-rc-service-back-edges
            s6-rc-service-upgrade

            user-processes-service-type

            assert-valid-graph))

;;; Commentary:
;;;
;;; Instantiating system services as a s6-rc configuration directory
;;;
;;; Code:


(define-record-type* <s6-rc-configuration>
  s6-rc-configuration make-s6-rc-configuration
  s6-rc-configuration?
  (s6-rc s6-rc-configuration-package
            (default s6-rc)) ; package
  (services s6-rc-configuration-services
            (default '()))) ; list of <s6-rc-service>

(define (s6-rc-boot-gexp config)
  "Return a gexp starting the s6-rc service."
  (let ((s6-rc (s6-rc-configuration-package config))
        (services (s6-rc-configuration-services config)))
  #~(begin
      ;; Keep track of the booted system.
      (false-if-exception (delete-file "/run/booted-system"))

      ;; Make /run/booted-system, an indirect GC root, point to the store item
      ;; /run/current-system points to.  Use 'canonicalize-path' rather than
      ;; 'readlink' to make sure we get the store item.
      (symlink (canonicalize-path "/run/current-system")
               "/run/booted-system")

      ;; Close any remaining open file descriptors to be on the safe
      ;; side.  This must be the very last thing we do, because
      ;; Guile has internal FDs such as 'sleep_pipe' that need to be
      ;; alive.
      (let loop ((fd 3))
        (when (< fd 1024)
          (false-if-exception (close-fdes fd))
          (loop (+ 1 fd))))

      ;; Start s6-rc.
      (execl #$(file-append s6-rc "/bin/s6-rc")
             "s6-rc" "--config"
             #$(s6-rc-configuration-file services s6-rc)))))

(define (s6-rc-launch-home-gexp config)
  (let* ((s6-rc (s6-rc-configuration-package config))
         (services (s6-rc-configuration-services config)))
    (if (s6-rc-configuration-auto-start? config)
        (with-imported-modules '((guix build utils))
          #~(let ((log-dir (or (getenv "XDG_LOG_HOME")
                               (format #f "~a/.local/var/log" (getenv "HOME")))))
              ((@ (guix build utils) mkdir-p) log-dir)
              (system*
               #$(file-append s6-rc "/bin/s6-rc-init")
               "--logfile"
               (string-append
                log-dir
                "/s6-rc.log")
               "--config"
               #$(s6-rc-configuration-file services s6-rc))))
        #~"")))

(define (s6-rc-reload-home-gexp config)
  (let* ((s6-rc (s6-rc-configuration-package config))
         (services (s6-rc-configuration-services config)))
    #~(system*
       #$(file-append s6-rc "/bin/s6-rc-init")
       "load" "root"
       #$(s6-rc-configuration-file services s6-rc))))

(define (s6-rc-ensure-home-gexp config)
  #~(if (file-exists?
         (string-append
          (or (getenv "XDG_RUNTIME_DIR")
              (format #f "/run/user/~a" (getuid)))
          "/s6-rc/socket"))
        #$(reload-configuration-gexp config)
        #$(launch-rc-gexp config)))

(define s6-rc-packages
  (compose list s6-rc-configuration-package))

(define s6-rc-root-service-type
  (service-type
   (name 's6-rc-root)
   ;; Extending the root s6-rc service (aka. PID 1) happens by
   ;; concatenating the list of services provided by the extensions.
   (compose concatenate)
   (extend (lambda (config extra-services)
             (s6-rc-configuration
               (inherit config)
               (services (append (s6-rc-configuration-services config)
                                 extra-services)))))
   (extensions (list (service-extension boot-service-type
                                        s6-rc-boot-gexp)
                     (service-extension profile-service-type
                                        s6-rc-packages)))
   (default-value (s6-rc-configuration))
   (description
    "Run the GNU Shepherd as PID 1---i.e., the operating system's first
process.  The Shepherd takes care of managing services such as daemons by
ensuring they are started and stopped in the right order.")))

(define %s6-rc-root-service
  ;; The root s6-rc service, aka. PID 1.  Its parameter is a
  ;; <s6-rc-configuration>.
  (service s6-rc-root-service-type))

(define-syntax s6-rc-service-type
  (syntax-rules (description)
    "Return a <service-type> denoting a simple s6-rc service--i.e., the type
for a service that extends SHEPHERD-ROOT-SERVICE-TYPE and nothing else.  When
DEFAULT is given, use it as the service's default value."
    ((_ service-name proc default (description text))
     (service-type
      (name service-name)
      (extensions
       (list (service-extension s6-rc-root-service-type
                                (compose list proc))))
      (default-value default)
      (description text)))
    ((_ service-name proc (description text))
     (service-type
      (name service-name)
      (extensions
       (list (service-extension s6-rc-root-service-type
                                (compose list proc))))
      (description text)))))

(define s6-rc-home-service-type
  (service-type (name 's6-rc)
                (extensions
                 (list (service-extension
                        s6-run-on-first-login-service-type
                        s6-rc-launch-home-gexp)
                       (service-extension
                        s6-activation-service-type
                        s6-rc-ensure-home-gexp)
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

(define %default-imported-modules
  ;; Default set of modules imported for a service's consumption.
  '((guix build utils)
    (guix build syscalls)))

(define %default-modules
  ;; Default set of modules visible in a service's file.
  '((s6-rc service)
    (oop goops)
    ((guix build utils) #:hide (delete))
    (guix build syscalls)))

(define-record-type* <s6-rc-service>
  s6-rc-service make-s6-rc-service
  s6-rc-service?
  (name            s6-rc-service-name)                ;symbol
  (documentation   s6-rc-service-documentation        ;string
                   (default "[No documentation.]"))
  (type            s6-rc-service-type)                ;symbol {longrun,oneshot,bundle}
  (essential?      s6-rc-service-essential?           ;boolean
                   (default #f))
  (timeout-up      s6-rc-service-timeout-up           ;number (millis)
                   (default 0))
  (timeout-down    s6-rc-service-timeout-down         ;number (millis)
                   (default 0))
  (dependencies    s6-rc-service-dependencies         ;list of symbols
                   (default '()))
  (run             s6-rc-service-run                  ;gexp (procedure)
                   (default #f))
  (finish          s6-rc-service-finish               ;gexp (procedure)
                   (default #f))
  (notification-fd s6-rc-service-notification-fd      ;number
                   (default 0))
  (timeout-kill    s6-rc-service-timeout-kill         ;number (millis)
                   (default 0))
  (timeout-finish  s6-rc-service-timeout-finish       ;number (millis)
                   (default 0))
  (nosetsid        s6-rc-service-nosetsid             ;boolean
                   (default #f))
  (max-deaths      s6-rc-service-max-deaths           ;number
                   (default 0))
  (down-signal     s6-rc-service-down-signal          ;symbol
                   (default #f))
  (data            s6-rc-service-data                 ;package/file
                   (default #f))
  (env             s6-rc-service-env                  ;alist of strings
                   (default '()))
  (producer-for    s6-rc-service-producer-for         ;symbol
                   (default #f))
  (consumer-for    s6-rc-service-consumer-for         ;list of symbols
                   (default '()))
  (pipeline-name   s6-rc-service-pipeline-name        ;symbol
                   (default #f))
  (modules         s6-rc-service-modules              ;list of module names
                   (default %default-modules)))

(define-record-type* <s6-rc-action>
  s6-rc-action make-s6-rc-action
  s6-rc-action?
  (name          s6-rc-action-name)                   ;symbol
  (procedure     s6-rc-action-procedure)              ;gexp
  (documentation s6-rc-action-documentation))         ;string

(define (s6-rc-service-provision service)
  "Return the provisioned names of SERVICE."
  `(,(s6-rc-service-name service)
    ,@(if (s6-rc-service-pipeline-name service)
          (list (s6-rc-service-pipeline-name service))
          '())))

(define (assert-valid-graph services)
  "Raise an error if SERVICES does not define a valid s6-rc service graph,
for instance if a service requires a nonexistent service, or if more than one
service uses a given name.

These are constraints that s6-rc's 'register-service' verifies but we'd
better verify them here statically than wait until PID 1 halts with an
assertion failure."
  (define provisions
    ;; The set of provisions (symbols).  Bail out if a symbol is given more
    ;; than once.
    (fold (lambda (service set)
            (define (assert-unique symbol)
              (when (set-contains? set symbol)
                (raise (condition
                        (&message
                         (message
                          (format #f (G_ "service '~a' provided more than once")
                                  symbol)))))))

            (define (assert-valid symbol)
              (when (or (string-prefix-ci? "s6-rc-" (symbol->string symbol))
                        (string-prefix-ci? "s6rc-" (symbol->string symbol)))
                (raise (condition
                        (&message
                         (message
                          (format #f (G_ "service '~a' has an illegal name")
                                  symbol)))))))

            (for-each assert-unique (s6-rc-service-provision service))
            (for-each assert-valid (s6-rc-service-provision service))
            (fold set-insert set (s6-rc-service-provision service)))
          (setq 's6-rc)
          services))

  (define (assert-satisfied-requirements service)
    ;; Bail out if the requirements of SERVICE aren't satisfied.
    (for-each (lambda (requirement)
                (unless (set-contains? provisions requirement)
                  (raise (condition
                          (&message
                           (message
                            (format #f (G_ "service '~a' requires '~a', \
which is not provided by any service")
                                    (match (s6-rc-service-provision service)
                                      ((head . _) head)
                                      (_          service))
                                    requirement)))))))
              (s6-rc-service-dependencies service)))

  (for-each assert-satisfied-requirements services))

(define %store-characters
  ;; Valid store characters; see 'checkStoreName' in the daemon.
  (string->char-set
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+-._?="))

(define (s6-rc-service-dir-name service)
  "Return the dir name where the initialization code for SERVICE is to be
stored."
  (let ((name (symbol->string (s6-rc-service-name service))))
    (string-append (string-map (lambda (chr)
                                 (if (char-set-contains? %store-characters chr)
                                     chr
                                     #\-))
                               name)
                   "-svdir")))

(define (s6-rc-service-dir service)
  "Return a dir defining SERVICE."
  (let ((run (program-file "run"
               (with-imported-modules %default-imported-modules
                 #~(begin
                     (use-modules #$@(s6-rc-service-modules service))
 
                     #$@(s6-rc-service-run service)))))
        (finish (program-file "finish"
                  (with-imported-modules %default-imported-modules
                    #~(begin
                        (use-modules #$@(s6-rc-service-modules service))
                   
                        #$@(s6-rc-service-finish service)))))
        (empty (plain-file "empty" "")))
    (file-union (s6-rc-service-dir-name service)
      (append
        `(("type" ,(let ((name (symbol->string (s6-rc-service-name service)))
                         (type (symbol->string (s6-rc-service-type service))))
                     (plain-file (string-append name "-type") type)))
          ,@(if (s6-rc-service-essential? service)
                `(("flag-essential" ,empty))
                '())
          ,@(if (s6-rc-service-dependencies service)
                `((,(if (eq? (s6-rc-service-type service) 'bundle)
                        "contents" "dependencies")
                    ,(plain-file "dependencies"
                       (string-join
                         (sort
                           (map symbol->string
                             (s6-rc-service-dependencies service))
                           string<?)
                         "\n"))))
                '()))
        (match (s6-rc-service-type service)
          ('oneshot
           `(("up" ,(mixed-text-file "up" run))
             ,@(if (s6-rc-service-finish service)
                   `(("down" ,(mixed-text-file "down" finish)))
                   '())
             ,@(if (not (zero? (s6-rc-service-timeout-up service)))
                   `(("timeout-up" ,(plain-file "timeout"
                                      (number->string
                                        (s6-rc-service-timeout-up service)))))
                   '())
             ,@(if (not (zero? (s6-rc-service-timeout-down service)))
                   `(("timeout-down" ,(plain-file "timeout"
                                        (number->string
                                          (s6-rc-service-timeout-down service)))))
                   '())))
          ('longrun
           `(("run" ,run)
             ,@(if (s6-rc-service-finish service) `(("finish" ,finish)) '())
             ,@(if (not (zero? (s6-rc-service-notification-fd service)))
                   `(("notification-fd" ,(plain-file "notif-fd"
                                           (number->string
                                             (s6-rc-service-notification-fd service)))))
                   '())
             ,@(if (not (zero? (s6-rc-service-timeout-kill service)))
                   `(("timeout-kill" ,(plain-file "timeout"
                                        (number->string
                                          (s6-rc-service-timeout-kill service)))))
                   '())
             ,@(if (not (zero? (s6-rc-service-timeout-finish service)))
                   `(("timeout-finish" ,(plain-file "timeout"
                                          (number->string
                                            (s6-rc-service-timeout-finish service)))))
                   '())
             ,@(if (s6-rc-service-nosetsid service) `(("nosetsid" ,empty)) '())
             ,@(if (not (zero? (s6-rc-service-max-deaths service)))
                   `(("max-death-tally" ,(plain-file "max-tally"
                                           (number->string
                                             (s6-rc-service-max-deaths service)))))
                   '())
             ,@(if (s6-rc-service-down-signal service)
                   `(("down-signal" ,(plain-file "signal"
                                       (symbol->string
                                         (s6-rc-service-down-signal service)))))
                   '())
             ,@(if (s6-rc-service-data service)
                   `(("data" ,(s6-rc-service-data service)))
                   '())
             ,@(map (lambda (elt)
                      (let ((key (car elt))
                            (value (cdr elt)))
                        `(,(string-append "env/" key) ,(plain-file key value))))
                    (s6-rc-service-env service))
             ,@(if (s6-rc-service-producer-for service)
                   `(("producer-for" ,(plain-file "producers"
                                        (symbol->string
                                          (s6-rc-service-producer-for service)))))
                   '())
             ,@(if (s6-rc-service-consumer-for service)
                   `(("consumer-for" ,(plain-file "consumers"
                                        (string-join
                                          (sort
                                            (map symbol->string
                                              (s6-rc-service-consumer-for service))
                                            string<?)
                                          "\n"))))
                   '())
             ,@(if (s6-rc-service-pipeline-name service)
                   `(("pipeline-name" ,(plain-file "pipeline"
                                         (symbol->string
                                           (s6-rc-service-pipeline-name service)))))
                   '())))
          ('bundle
           `())
          (_ (raise (condition
                      (&message
                        (message
                          (format #f (G_ "service '~a' has an invalid type: ~a")
                                  symbol (s6-rc-service-type service))))))))))))

(define* (s6-rc-oneshot-service services
                                      #:key
                                      (provision s6-rc-service-provision)
                                      (requirement s6-rc-service-requirement))
  "Return a procedure that, when given a <s6-rc-service> from SERVICES,
returns the list of <s6-rc-service> that depend on it.

Use PROVISION and REQUIREMENT as one-argument procedures that return the
symbols provided/required by a service."
  (define provision->service
    (s6-rc-service-lookup-procedure services provision))

  (define edges
    (fold (lambda (service edges)
            (fold (lambda (requirement edges)
                    (vhash-consq (provision->service requirement) service
                                 edges))
                  edges
                  (requirement service)))
          vlist-null
          services))

  (lambda (service)
    (vhash-foldq* cons '() service edges)))

(define* (s6-rc-configuration-dir services #:optional (s6-rc s6-rc))
  "Return the s6-rc configuration dir for SERVICES.  S6-RC is used
as s6-rc package."
  (assert-valid-graph services)

  (let* ((source (file-union "s6-rc"
                   (map (lambda (s)
                          `(,(symbol->string (s6-rc-service-name s))
                             ,(s6-rc-service-dir s)))
                        services)))
         (compiled (computed-file "s6-rc-compiled"
                     (with-imported-modules %default-imported-modules
                       #~(begin
                           (use-modules (guix build utils))
                      
                           (invoke #$(file-append s6-rc "/bin/s6-rc-compile")
                                   "-v" "3" #$output #$source))))))
    (define config
      #~(begin
          (use-modules (srfi srfi-34)
                       (system repl error-handling))

          (default-environment-variables
            '("PATH=/run/current-system/profile/bin"))

          (format #t "starting services...~%")

          (redirect-port (open-input-file "/dev/null")
                         (current-input-port))))

    (file-union "s6-rc-tree"
      `(("sv" ,source)
        ("rc" ,compiled)))))

(define* (s6-rc-service-lookup-procedure services
                                            #:optional
                                            (provision
                                             s6-rc-service-provision))
  "Return a procedure that, when passed a symbol, return the item among
SERVICES that provides this symbol.  PROVISION must be a one-argument
procedure that takes a service and returns the list of symbols it provides."
  (let ((services (fold (lambda (service result)
                          (fold (cut vhash-consq <> service <>)
                                result
                                (provision service)))
                        vlist-null
                        services)))
    (lambda (name)
      (match (vhash-assq name services)
        ((_ . service) service)
        (#f            #f)))))

(define* (s6-rc-service-back-edges services
                                      #:key
                                      (provision s6-rc-service-provision)
                                      (requirement s6-rc-service-requirement))
  "Return a procedure that, when given a <s6-rc-service> from SERVICES,
returns the list of <s6-rc-service> that depend on it.

Use PROVISION and REQUIREMENT as one-argument procedures that return the
symbols provided/required by a service."
  (define provision->service
    (s6-rc-service-lookup-procedure services provision))

  (define edges
    (fold (lambda (service edges)
            (fold (lambda (requirement edges)
                    (vhash-consq (provision->service requirement) service
                                 edges))
                  edges
                  (requirement service)))
          vlist-null
          services))

  (lambda (service)
    (vhash-foldq* cons '() service edges)))

(define (s6-rc-service-upgrade live target)
  "Return two values: the subset of LIVE (a list of <live-service>) that needs
to be unloaded, and the subset of TARGET (a list of <s6-rc-service>) that
need to be restarted to complete their upgrade."
  (define (essential? service)
    (memq (first (live-service-provision service))
          '(root s6-rc)))

  (define lookup-target
    (s6-rc-service-lookup-procedure target
                                       s6-rc-service-provision))

  (define lookup-live
    (s6-rc-service-lookup-procedure live
                                       live-service-provision))

  (define (running? service)
    (and=> (lookup-live (s6-rc-service-canonical-name service))
           live-service-running))

  (define live-service-dependents
    (s6-rc-service-back-edges live
                                 #:provision live-service-provision
                                 #:requirement live-service-requirement))

  (define (obsolete? service)
    (match (lookup-target (first (live-service-provision service)))
      (#f (every obsolete? (live-service-dependents service)))
      (_  #f)))

  (define to-restart
    ;; Restart services that are currently running.
    (filter running? target))

  (define to-unload
    ;; Unload services that are no longer required.
    (remove essential? (filter obsolete? live)))

  (values to-unload to-restart))


;;;
;;; User processes.
;;;

(define (user-processes-s6-rc-service requirements)
  "Return the 'user-processes' Shepherd service with dependencies on
REQUIREMENTS (a list of service names).

This is a synchronization point used to make sure user processes and daemons
get started only after crucial initial services have been started---file
system mounts, etc.  This is similar to the 'sysvinit' target in systemd."
  (list (s6-rc-service
         (name 'user-processes)
         (documentation "When stopped, terminate all user processes.")
         (type 'bundle)
         (dependencies requirements)
         (max-deaths 1))))

(define user-processes-service-type
  (service-type
   (name 'user-processes)
   (extensions (list (service-extension s6-rc-root-service-type
                                        user-processes-s6-rc-service)))
   (compose concatenate)
   (extend append)

   ;; The value is the list of Shepherd services 'user-processes' depends on.
   ;; Extensions can add new services to this list.
   (default-value '())

   (description "The @code{user-processes} service is responsible for
terminating all the processes so that the root file system can be re-mounted
read-only, just before rebooting/halting.  Processes still running after a few
seconds after @code{SIGTERM} has been sent are terminated with
@code{SIGKILL}.")))

(map s6-rc-service-dir (user-processes-s6-rc-service '(a b c)))
(s6-rc-configuration-dir (user-processes-s6-rc-service '()))
;;; s6-rc.scm ends here
