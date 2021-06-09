(define-module (rc packages pipewire-next)
               #:use-module (guix packages)
               #:use-module (gnu packages linux)
               #:use-module (gnu packages pulseaudio)
               #:export (pipewire-next))

(define pipewire-next
  (package
    (inherit pipewire-0.3)
    (version "0.3.29")
    (source (package-source pipewire-0.3))))

(define-public pulseaudio->pipewire
  (package-input-rewriting
    `((,pulseaudio . ,pipewire-next))
    #:deep? #t))
