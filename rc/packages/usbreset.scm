(define-module (rc packages usbreset)
  #:use-module (rc packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:export (usbreset))

(define usbreset
  (package
    (name "usbreset")
    (version "0.0.1")
    (source #f)
    (build-system gnu-build-system)
    (arguments
      '(#:phases
        (modify-phases %standard-phases
          (delete 'unpack)
          (delete 'configure)
          (replace 'build (lambda* (#:key inputs outputs #:allow-other-keys)
                            (let* ((src (assoc-ref inputs "usbreset.c"))
                                   (out (assoc-ref outputs "out"))
                                   (bin (string-append out "/bin"))
                                   (targ (string-append bin "/usbreset")))
                              (mkdir-p bin)
                              (invoke "gcc" src "-o" targ))))
          (delete 'install))
        #:tests? #f))
    (inputs
      `(("usbreset.c" ,(local-file "../../data/usbreset.c" "usbreset.c"))))
    (description "")
    (synopsis "")
    (home-page "")
    (license #f)))

usbreset
