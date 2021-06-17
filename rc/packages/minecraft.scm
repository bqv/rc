(define-module (rc packages minecraft)
  #:use-module (rc packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages java)
  #:export (minecraft-server))

(define* (make-minecraft-server
           #:key (mc-version "1.17")
                 (mc-objectid "0a269b5f2c5b93b1712d0f5dc43b6182b9ab254e") ; sha-1 hash
                 (src-hash (base32 "0jqz7hpx7zvjj2n5rfrh8jmdj6ziqyp8c9nq4sr4jmkbky6hsfbv")))
  (package
    (name "minecraft-server")
    (version mc-version)
    (source (origin
              (method url-fetch)
              (uri (string-append "https://launcher.mojang.com/v1/objects/"
                                  mc-objectid
                                  "/server.jar"))
              (sha256 src-hash)))
    (build-system trivial-build-system)
    (inputs `(("openjdk" ,openjdk16)
              ("bash"    ,bash)))
    (arguments
      `(#:modules ((guix build utils))
        #:builder (begin
                    (use-modules (guix build utils)
                                 (srfi srfi-26))
                    (let* ((source   (assoc-ref %build-inputs "source"))
                           (patchelf (assoc-ref %build-inputs "patchelf"))
                           (openjdk  (assoc-ref %build-inputs "openjdk"))
                           (bash     (assoc-ref %build-inputs "bash"))
                           (output   (assoc-ref %outputs "out")))
                      (mkdir-p (string-append output "/lib/minecraft"))
                      (copy-file source (string-append output "/lib/minecraft/server.jar"))
                      (mkdir-p (string-append output "/bin")) 
                      ;; TODO: replace this with lisp (use install-jars, etc)
                      (let ((port (open-file (string-append output "/bin/minecraft-server") "a")))
                        (display (string-append "#!" (string-append bash "/bin/sh") "\n") port)
                        (display (string-append "exec "
                                                (string-append openjdk "/bin/java ")
                                                "-jar "
                                                "$@ "
                                                (string-append output "/lib/minecraft/server.jar ")
                                                "nogui "
                                                "\n") port)
                        ;; TODO: use some syntax sugar like "with-output-to-port"?
                        (close port))
                      (chmod (string-append output "/bin/minecraft-server") #o555) 
                      #t))))
    (home-page "https://minecraft.net")
    (synopsis "Minecraft Server")
    (description "Minecraft Server")
    (license #f)))

(define minecraft-server
  (make-minecraft-server))

minecraft-server