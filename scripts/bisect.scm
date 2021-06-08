#!/usr/bin/env -S guile --no-auto-compile
;; Mode: -*- Scheme;-*-
!#

(use-modules (gnu)
             (git repository)
             (git reference)
             (git oid)
             (git commit)
             (git describe)
             (guix git)
             (guix packages)
             (guix utils)
             (guix gexp)
             (guix monads)
             (guix store)
             (guix derivations)
             ((guix build utils) #:select (alist-replace))
             (srfi srfi-71)
             (rnrs io ports))
(use-package-modules nss sqlite tcl)

(define* (describe-checkout checkout
                           #:optional (options (make-describe-options
                                                ;; Consider unannotated tags.
                                                #:strategy 'tags
                                                ;; ...but not their ancestors.
                                                #:only-follow-first-parent? #t)))
 "Get the current HEAD of CHECKOUT as well as its \"pretty name\"."
  (let* ((repo (repository-open checkout))
         (head (reference-target (repository-head repo)))
         (commit (commit-lookup repo head))
         (description (describe-commit commit options)))
    (repository-close! repo)
    (values (oid->string head) (describe-format description))))

(define %repository
  (string-append
    (getenv "HOME")
    "/src/sqlite-git"))

(define sqlite/bisect
  (let ((commit pretty (describe-checkout %repository)))
    (package
      (inherit sqlite)
      (source (git-checkout (url %repository) (commit commit)))
      ;; Drop the -version prefix from the tag name.
      (version (string-drop pretty 8))
      (arguments
       (substitute-keyword-arguments (package-arguments sqlite)
         ((#:configure-flags flags ''())
          ;; TCL is needed for creating the amalgamation, but we
          ;; don't want to install the extension.
          `(cons "--disable-tcl" ,flags))
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'configure 'make-amalgamation
               (lambda _
                 (invoke "make" "sqlite3.c" "sqlite3.h")))))
         ;; Don't bother trying the tests.
         ((#:tests? _ #f) #f)))
      (native-inputs
       `(("tcl" ,tcl)
         ,@(package-native-inputs sqlite))))))

(define nss/bisect
  (package
    (inherit nss)
    (inputs (alist-replace "sqlite" (list sqlite/bisect)
                           (package-inputs nss)))
    (arguments
     (substitute-keyword-arguments (package-arguments nss)
       ;; We're not interested in the test suite, as we know
       ;; which test is failing and run it below.
       ((#:tests? _ #f) #f)))))

(define %dbdir
  (string-append
    "/tmp/guix-build-nss-3.66.drv-0/"
    "nss-3.66/tests_results/security/localhost.1/ronlydir"))

(let ((result
       (false-if-exception
        (with-store store
          (run-with-store store
            (mlet* %store-monad ((nss (package->derivation nss/bisect))
                                 (_ (built-derivations (list nss))))
              (return (status:exit-val
                       (system* (string-append (derivation->output-path nss "bin")
                                               "/bin/dbtest")
                                "-d" %dbdir)))))))))
  (if result
      (exit result)
      (exit 125)))
