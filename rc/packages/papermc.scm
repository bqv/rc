(define-module (rc packages papermc)
  #:use-module (rc packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages java)
  #:export (papermc))

(define papermc
  (let ((mc-version "1.16.5")
        (build-num "771"))
    (package
      (name "papermc")
      (version (string-append mc-version "r" build-num))
      (source (origin
                (method url-fetch)
                (uri (string-append "https://papermc.io/api/v1/paper/"
                                    mc-version
                                    "/"
                                    build-num
                                    "/download"))
                (sha256
                  (base32
                    "1lmlfhigbzbkgzfq6knglka0ccf4i32ch25gkny0c5fllmsnm08l"))))
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
                        (mkdir-p (string-append output "/share/papermc"))
                        (copy-file source (string-append output "/share/papermc/papermc.jar"))
                        (mkdir-p (string-append output "/bin")) 
                        ;; TODO: replace this with lisp (use install-jars, etc)
                        (let ((port (open-file (string-append output "/bin/minecraft-server") "a")))
                          (display (string-append "#!" (string-append bash "/bin/sh") "\n") port)
                          (display (string-append "exec -a minecraft-server "
                                                  (string-append openjdk "/bin/java ")
                                                  "-jar "
                                                  (string-append output "/share/papermc/papermc.jar ")
                                                  "nogui "
                                                  "\n") port)
                          ;; TODO: use some syntax sugar like "with-output-to-port"?
                          (close port))
                        (chmod (string-append output "/bin/minecraft-server") #o555) 
                        #t))))
      (home-page "https://papermc.io")
      (synopsis "High-performance Minecraft Server")
      (description "High-performance Minecraft Server")
      (license license:gpl3))))

papermc
