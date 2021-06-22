(define-module (rc packages pipewire)
               #:use-module (guix git-download)
               #:use-module (guix utils)
               #:use-module (guix packages)
               #:use-module (gnu packages linux)
               #:use-module (gnu packages pulseaudio)
               #:export ())

(define-public pulseaudio->pipewire
  (package-input-rewriting
    `((,pulseaudio . ,pipewire-0.3))
    #:deep? #t))
