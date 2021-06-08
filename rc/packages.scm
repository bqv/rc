(define-module (rc packages)
  #:use-module ((rc) #:prefix rc:)
  #:use-module ((gnu packages) #:prefix gnu:)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:export (search-patch
            search-patches
            %patch-path))

(define %patch-path
  (make-parameter
   (append
    (list (string-append rc:%channel-root "/patches"))
    (gnu:%patch-path))))

(define (search-patch file-name)
  "Search the patch FILE-NAME.  Raise an error if not found."
  (or (search-path (%patch-path) file-name)
      (raise (formatted-message (G_ "~a: patch not found")
                                file-name))))

(define-syntax-rule (search-patches file-name ...)
  "Return the list of absolute file names corresponding to each
FILE-NAME found in %PATCH-PATH."
  (list (search-patch file-name) ...))
