(define-module (rc packages pipewire)
               #:use-module (guix git-download)
               #:use-module (guix utils)
               #:use-module (guix packages)
               #:use-module (gnu packages linux)
               #:use-module (gnu packages pulseaudio)
               #:export (pipewire-next))

(define pipewire-next
  (package
    (inherit pipewire-0.3)
    (version "0.3.29")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url (git-reference-url (origin-uri (package-source pipewire-0.3))))
                     (commit version)))
              (file-name (git-file-name (package-name pipewire-0.3) version))
              (sha256
                (base32
                  "16jjxcnahxqfcawz77ywx837ybhwzcivn7hgqb9cmlp1y2syy8gk"))))
    (arguments
     (map (lambda (a)
            (if (list? a)
                (map (lambda (f)
                       (if (string? f)
                           (string-replace-substring f
                                                     "systemd=false"
                                                     "systemd=disabled")
                           f))
                     a)
                a))
          (package-arguments pipewire-0.3)))))

(define-public pulseaudio->pipewire
  (package-input-rewriting
    `((,pulseaudio . ,pipewire-next))
    #:deep? #t))
