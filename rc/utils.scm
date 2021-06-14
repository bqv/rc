(define-module (rc utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1)
  #:export (list-directory
            file->package))

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
            (assoc-ref %outputs "out"))
          #t)))
    (synopsis synopsis)
    (description (format #f "~A" file))
    (home-page home-page)
    (license license)))
