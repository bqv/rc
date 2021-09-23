(define-module (rc utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix inferior)
  #:use-module (guix licenses)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages)
  #:export (list-directory
            file->package
            program-package
            inferior-package->package))

(define (list-directory path)
  (cddr (reverse
          (let ((s (opendir path))
                (l '()))
            (do ((d (readdir s) (readdir s)))
              ((eof-object? d))
              (set! l (cons d l)))
            (closedir s)
            l))))

(define* (file->package file
                        #:optional (name "file")
                                   (version "0")
                                   (bin #f)
                        #:key (synopsis "Generated file")
                              (home-page "")
                              (license #f))
  (package
    (name name)
    (source file)
    (version version)
    (build-system trivial-build-system)
    (arguments
      `(#:modules ((guix build utils))
        #:builder
        (begin
          (use-modules (guix build utils)
                       (srfi srfi-26))
          (copy-file
            (assoc-ref %build-inputs "source")
            (if ,bin
                (let ((bindir (string-append (assoc-ref %outputs "out") "/bin")))
                  (mkdir-p bindir)
                  (string-append bindir "/" ,name))
                (assoc-ref %outputs "out")))
          #t)))
    (synopsis synopsis)
    (description (format #f "~A" file))
    (home-page home-page)
    (license license)))

(define (program-package name gexp)
  (file->package
    (program-file name gexp)
    name "0" #t))

(define* (inferior-package->package inferior
                                    #:key
                                    (license (package-license
                                               (specification->package
                                                 (inferior-package-name inferior)))))
  (package
    (name (inferior-package-name inferior))
    (version (inferior-package-version inferior))
    (source inferior)
    (build-system (@ (guix build-system copy) copy-build-system))
    (synopsis (inferior-package-synopsis inferior))
    (description (inferior-package-description inferior))
    (license license)
    (home-page (inferior-package-home-page inferior))))
