(define-module (rc home-services pipewire)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (pipewire-configuration
            pipewire-configuration?
            pipewire-service-type
            pipewire-pulse-configuration
            pipewire-pulse-configuration?
            pipewire-pulse-service-type
            pipewire-media-session-configuration
            pipewire-media-session-configuration?
            pipewire-media-session-service-type))


(define-record-type* <pipewire-configuration>
  pipewire-configuration make-pipewire-configuration
  pipewire-configuration?
  (package  pipewire-configuration-package  ;<package>
            (default pipewire))
  (config   pipewire-configuration-file     ;<file>
            (default (plain-file "pipewire.conf" ""))))

(define pipewire-shepherd-service
  (match-lambda
    (($ <pipewire-configuration> pipewire config)
     (list
       (shepherd-service
         (provision '(pipewire))
         (requirement '(dbus))
         (documentation "Run pipewire.")
         (start #~(make-forkexec-constructor
                    (list (string-append #$pipewire "/bin/pipewire")
                         ;#$config
                          )))
         (respawn? #t)
         (stop #~(make-kill-destructor)))))))

(define pipewire-service-type
  (service-type (name 'pipewire)
                (extensions (list (service-extension home-shepherd-service-type
                                                     pipewire-shepherd-service)))
                (description "Run pipewire.")))


(define-record-type* <pipewire-pulse-configuration>
  pipewire-pulse-configuration make-pipewire-pulse-configuration
  pipewire-pulse-configuration?
  (package  pipewire-pulse-configuration-package  ;<package>
            (default pipewire))
  (config   pipewire-pulse-configuration-file     ;<file>
            (default (plain-file "pipewire-pulse.conf" ""))))

(define pipewire-pulse-shepherd-service
  (match-lambda
    (($ <pipewire-pulse-configuration> pipewire config)
     (list
       (shepherd-service
         (provision '(pipewire-pulse))
         (requirement '(pipewire))
         (documentation "Run pipewire-pulse.")
         (start #~(make-forkexec-constructor
                    (list (string-append #$pipewire "/bin/pipewire-pulse")
                         ;#$config
                          )))
         (respawn? #t)
         (stop #~(make-kill-destructor)))))))

(define pipewire-pulse-service-type
  (service-type (name 'pipewire-pulse)
                (extensions (list (service-extension home-shepherd-service-type
                                                     pipewire-pulse-shepherd-service)))
                (description "Run pipewire-pulse.")))


(define-record-type* <pipewire-media-session-configuration>
  pipewire-media-session-configuration make-pipewire-media-session-configuration
  pipewire-media-session-configuration?
  (package  pipewire-media-session-configuration-package  ;<package>
            (default pipewire)))

(define pipewire-media-session-shepherd-service
  (match-lambda
    (($ <pipewire-media-session-configuration> pipewire)
     (list
       (shepherd-service
         (provision '(pipewire-media-session))
         (requirement '(pipewire))
         (documentation "Run pipewire-media-session.")
         (start #~(make-forkexec-constructor
                    (list (string-append #$pipewire "/bin/pipewire-media-session"))))
         (respawn? #t)
         (stop #~(make-kill-destructor)))))))

(define pipewire-media-session-service-type
  (service-type (name 'pipewire-media-session)
                (extensions (list (service-extension home-shepherd-service-type
                                                     pipewire-media-session-shepherd-service)))
                (description "Run pipewire-media-session.")))
