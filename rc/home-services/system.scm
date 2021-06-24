(define-module (rc home-services system)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu services)
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

(define null-service-type
  (service-type
    (name 'null)
    (extensions (list))
    (default-value #f)
    (description "")))
(simple-service 'nil null-service-type #t)

(define system-service-type
  (service-type (name 'system-service)
                (extensions (list))
                (default-value '())
                (description "Add system-level services.")))
