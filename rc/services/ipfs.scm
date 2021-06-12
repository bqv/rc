(define-module (rc services ipfs)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services linux)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services dbus)
  #:use-module (gnu system shadow)
  #:use-module (gnu system pam)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cluster)
  #:use-module (gnu packages connman)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages tor)
  #:use-module (gnu packages usb-modeswitch)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ntp)
  #:use-module (gnu packages wicd)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages ipfs)
  #:use-module (gnu build linux-container)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
  #:use-module (rnrs enums)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:replace (ipfs-service-type
             ipfs-configuration
             ipfs-configuration?
             ipfs-configuration-package
             ipfs-configuration-gateway
             ipfs-configuration-api))

;;;
;;; IPFS
;;;

(define-record-type* <ipfs-configuration>
  ipfs-configuration
  make-ipfs-configuration
  ipfs-configuration?
  (package  ipfs-configuration-package
            (default go-ipfs))
  (gateway  ipfs-configuration-gateway
            (default "/ip4/127.0.0.1/tcp/8082"))
  (api      ipfs-configuration-api
            (default "/ip4/127.0.0.1/tcp/5001"))
  (migrate  ipfs-configuration-migrate
            (default #f))
  (mount    ipfs-configuration-mount
            (default #f))
  (args     ipfs-configuration-args
            (default '()))
  (settings ipfs-configuration-settings
            (default '())))

(define %ipfs-home "/var/lib/ipfs")
(define %ipfs-mount-ipfs "/ipfs")
(define %ipfs-mount-ipns "/ipns")

(define %ipfs-accounts
  (list (user-account
         (name "ipfs")
         (group "ipfs")
         (system? #t)
         (comment "IPFS daemon user")
         (home-directory %ipfs-home)
         (shell (file-append shadow "/sbin/nologin")))
        (user-group
         (name "ipfs")
         (system? #t))))

(define (ipfs-binary config)
  (file-append (ipfs-configuration-package config) "/bin/ipfs"))

(define %ipfs-home-mapping
  #~(file-system-mapping
     (source #$%ipfs-home)
     (target #$%ipfs-home)
     (writable? #t)))

(define %ipfs-mount-ipfs-mapping
  #~(file-system-mapping
     (source #$%ipfs-mount-ipfs)
     (target #$%ipfs-mount-ipfs)
     (writable? #t)))

(define %ipfs-mount-ipns-mapping
  #~(file-system-mapping
     (source #$%ipfs-mount-ipns)
     (target #$%ipfs-mount-ipns)
     (writable? #t)))

(define %ipfs-environment
  #~(list #$(string-append "HOME=" %ipfs-home)
          #$(string-append "IPFS_PATH=" %ipfs-home)
          (string-append "PATH=" #$(file-append fuse "/bin") ":" (getenv "PATH"))))

(define %ipfs-global-environment
  `(("IPFS_PATH" . ,%ipfs-home)))

(define (ipfs-shepherd-service config)
  "Return a <shepherd-service> for IPFS with CONFIG."
  (define ipfs-daemon-command
    #~(list #$(ipfs-binary config) "daemon"
            #$@(if (ipfs-configuration-migrate config)
                   (list "--migrate") (list))
            #$@(if (ipfs-configuration-mount config)
                   (list "--mount") (list))
            #$@(ipfs-configuration-args config)))
  (list
   (with-imported-modules (source-module-closure
                           '((gnu build shepherd)
                             (gnu system file-systems)))
     (shepherd-service
      (provision '(ipfs))
      ;; While IPFS is most useful when the machine is connected
      ;; to the network, only loopback is required for starting
      ;; the service.
      (requirement '(loopback))
      (documentation "Connect to the IPFS network")
      (modules '((gnu build shepherd)
                 (gnu system file-systems)))
      (start #~(make-forkexec-constructor/container
                #$ipfs-daemon-command
                #:namespaces '#$(fold delq %namespaces '(user net))
                #:mappings (list #$%ipfs-home-mapping
                                 #$%ipfs-mount-ipfs-mapping
                                 #$%ipfs-mount-ipns-mapping)
                #:log-file "/var/log/ipfs.log"
                #:user "ipfs"
                #:group "ipfs"
                #:environment-variables #$%ipfs-environment))
      (stop #~(make-kill-destructor))))))

(define (%ipfs-activation config)
  "Return an activation gexp for IPFS with CONFIG"
  (define (ipfs-config-command setting value)
    #~(#$(ipfs-binary config) "--offline" "config" #$setting #$value))
  (define (set-config!-gexp setting value)
    #~(system* #$@(ipfs-config-command setting value)))
  (define settings
    `(("Addresses.API" ,(ipfs-configuration-api config))
      ("Addresses.Gateway" ,(ipfs-configuration-gateway config))
      ,@(ipfs-configuration-settings config)))
  (define inner-gexp
    #~(begin
        (umask #o077)
        ;; Recover old ipfs repo structure
        (false-if-exception
          (let ((dir #$(string-append %ipfs-home "/.ipfs")))
            (when (file-exists? dir)
              (let ((port (opendir dir)))
                (do ((entry (readdir port) (readdir port)))
                  ((eof-object? entry))
                  (unless (member entry (list "." ".."))
                    (rename-file (string-append dir "/" entry)
                                 (string-append #$%ipfs-home "/" entry))))
                (closedir port))
              (rmdir dir))))
        ;; Create ipfs repo structure if not exists
        (unless (file-exists? #$(string-append %ipfs-home "/config"))
          (system* #$(ipfs-binary config) "init"))
        ;; Apply settings
        #$@(map (cute apply set-config!-gexp <>) settings)
        ;; Fix permissions
        (chmod #$(string-append %ipfs-home) #o711)
        (chmod #$(string-append %ipfs-home "/config") #o644)))
  (define inner-script
    (program-file "ipfs-activation-inner" inner-gexp))
  ;; Run ipfs init and ipfs config from a container,
  ;; in case the IPFS daemon was compromised at some point
  ;; and ~/.ipfs is now a symlink to somewhere outside
  ;; %ipfs-home.
  (define container-gexp
    (with-extensions (list shepherd)
      (with-imported-modules (source-module-closure
                              '((gnu build shepherd)
                                (gnu system file-systems)))
        #~(begin
            (use-modules (gnu build shepherd)
                         (gnu system file-systems))
            (let* ((constructor
                    (make-forkexec-constructor/container
                     (list #$inner-script)
                     #:namespaces '#$(fold delq %namespaces '(user))
                     #:mappings (list #$%ipfs-home-mapping)
                     #:user "ipfs"
                     #:group "ipfs"
                     #:environment-variables #$%ipfs-environment))
                   (pid (constructor)))
              (waitpid pid))))))
  ;; The activation may happen from the initrd, which uses
  ;; a statically-linked guile, while the guix container
  ;; procedures require a working dynamic-link.
  (define container-script
    (program-file "ipfs-activation-container" container-gexp))
  #~(begin
      (mkdir-p #$%ipfs-mount-ipfs)
      (mkdir-p #$%ipfs-mount-ipns)
      (let ((pwd (getpwnam "ipfs")))
        (chown #$%ipfs-mount-ipfs (passwd:uid pwd) (passwd:gid pwd))
        (chown #$%ipfs-mount-ipns (passwd:uid pwd) (passwd:gid pwd)))
      (system* #$container-script)))

(define ipfs-service-type
  (service-type
   (name 'ipfs)
   (extensions
    (list (service-extension account-service-type
                             (const %ipfs-accounts))
          (service-extension activation-service-type
                             %ipfs-activation)
          (service-extension session-environment-service-type
                             (const %ipfs-global-environment))
          (service-extension shepherd-root-service-type
                             ipfs-shepherd-service)))
   (default-value (ipfs-configuration))
   (description
    "Run @command{ipfs daemon}, the reference implementation
of the IPFS peer-to-peer storage network.")))
